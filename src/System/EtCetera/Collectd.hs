{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module System.EtCetera.Collectd where

-- Based directly on:
--  https://github.com/collectd/collectd/blob/master/src/liboconfig/parser.y
--  https://github.com/collectd/collectd/blob/master/src/liboconfig/scanner.l

import           Control.Category ((.), id)
import           Data.Char (ord)
import           Data.Function (on)
import           Data.List (intersperse)
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import           Data.Ord (compare, Ordering(..))
import           Numeric (showHex, showOct)
import           Prelude hiding ((.), id)
import           Text.Boomerang.Combinators (duck1, manyl, manyr, opt, push, rCons, rList, rList1, rListSep,
                                             rNil, somel, chainl, chainr, rPair)
import           Text.Boomerang.Error (condenseErrors, mkParserError, ErrorMsg(..))
import           Text.Boomerang.HStack (arg, (:-)(..))
import           Text.Boomerang.Prim (bestErrors, Boomerang(..), Parser(..), xmaph, xpure, runParser, prs,
                                      val)
import           Text.Boomerang.Pos (ErrorPosition(..), InitialPosition(..), MajorMinorPos, Pos)
import           Text.Boomerang.String (alpha, anyChar, char, digit, lit, satisfy, StringBoomerang(..),
                                        StringError)

type Name = String
type Arg = String

data Var = Var Name [Arg] [Var]

oneOf :: String -> StringBoomerang r (Char :- r)
oneOf l = satisfy (`elem` l)

noneOf :: String -> StringBoomerang r (Char :- r)
noneOf l = satisfy (not . (`elem` l))

digitInRange :: Int -> Int -> StringBoomerang r (Char :- r)
digitInRange s e = oneOf . concatMap show $ [s..e]

alphaInRange :: Char -> Char -> StringBoomerang r (Char :- r)
alphaInRange s e = oneOf . concatMap show $ [ord s..ord e]

whiteSpace :: StringBoomerang r r
whiteSpace = lit " " <> lit "\t" <> lit "\b" <> lit "\\\n" <> lit "\\\r\n"

eol :: StringBoomerang r r
eol = lit "\n" <> lit "\r\n"

eolOrComment :: StringBoomerang r r
eolOrComment = comment <> eol

data QuotedStringPart = Escaped String | Literal String
  deriving (Eq, Show)
type QuotedString = [QuotedStringPart]


-- | Converts a router for a value @a@ to a router for a list of @a@, with a separator.
rListSep' :: Boomerang e tok r (a :- r) -> Boomerang e tok ([a] :- r) ([a] :- r) -> Boomerang e tok r ([a] :- r)
rListSep' r sep = chainl (rCons . duck1 r) sep . rNil


quotedString :: StringBoomerang r (QuotedString :- r)
quotedString =
  lit "\"" .
    ((opt (rCons . esc) . chrsEscRec) <>
     (opt (rCons . chrs) . escChrsRec) <>
     (rCons . esc . rNil) <>
     (rCons . chrs . rNil)
     ) .
  lit "\""
 where
  chrsEscRec :: StringBoomerang r (QuotedString :- r)
  chrsEscRec = rCons . chrs . rCons . esc . (chrsEscRec <> rNil)

  escChrsRec :: StringBoomerang r (QuotedString :- r)
  escChrsRec = rCons . esc . rCons . chrs . (escChrsRec <> rNil)

  chrs :: StringBoomerang r (QuotedStringPart :- r)
  chrs =
    xmaph Literal fromLiteral (rList1 (noneOf "\\\""))
   where
    fromLiteral qsp =
      case qsp of
        Literal s -> Just s
        otherwise -> Nothing

  esc :: StringBoomerang r (QuotedStringPart :- r)
  esc =
    xmaph Escaped fromEscaped (rList1 (lit "\\\n" . delete (rList1 (oneOf " \t")) . push '\t' <> lit "\\" . anyChar))
   where
    fromEscaped qsp =
      case qsp of
        Escaped s -> Just s
        otherwise -> Nothing

unquotedString :: StringBoomerang r (Value :- r)
unquotedString =
  xpure (arg (:-) StringValue) serializer . rList1 (alpha <> digit)
 where
  serializer :: (Value :- r) -> Maybe (String :- r)
  serializer (StringValue s :- r) = Just (s :- r)
  serializer _                    = Nothing


-- HEX_NUMBER 0[xX][0-9a-fA-F]+
hexNumber :: StringBoomerang r (Value :- r)
hexNumber =
  hex . (rCons . char '0') . (rCons  . oneOf "xX") .
    rList1 (digit <> alphaInRange 'a' 'f' <> alphaInRange 'A' 'F')
 where
  hex = xpure (arg (:-) (IntValue . read)) intValSerializer

  intValSerializer :: (Value :- r) -> Maybe (String :- r)
  intValSerializer (IntValue x :- r) = Just (("0x" ++ showHex x "") :- r)
  intValSerializer _                 = Nothing

-- OCT_NUMBER 0[0-7]+
octalNumber :: StringBoomerang r (Value :- r)
octalNumber =
  octal . (rCons . char '0' . rCons . push 'o') . rList1  (digitInRange 0 7)
 where
  octal = xpure (arg (:-) (IntValue . read)) intValSerializer

  intValSerializer :: (Value :- r) -> Maybe (String :- r)
  intValSerializer (IntValue x :- r) = Just (("0x" ++ showOct x "") :- r)
  intValSerializer _                 = Nothing

-- DEC_NUMBER [\+\-]?[0-9]+
decNumber :: StringBoomerang r (Value :- r)
decNumber =
  int . opt (rCons . oneOf "+-") . rList1 digit
 where
  int = xpure (arg (:-) (\x -> case x of
                                    ('+':r)   -> IntValue (read r)
                                    otherwise -> IntValue (read x)))
                  intValSerializer
  intValSerializer :: (Value :- r) -> Maybe (String :- r)
  intValSerializer (IntValue x :- r) = Just (show x :- r)
  intValSerializer _                 = Nothing

-- FLOAT_NUMBER [\+\-]?[0-9]*\.[0-9]+([eE][\+\-][0-9]+)?
floatNumber :: StringBoomerang r (Value :- r)
floatNumber =
  readFloat . ife
 where
  -- hacky way to parse float through `read`
  rJoin :: ([a] -> ([a], [a])) -> Boomerang e tok ([a] :- [a] :- r) ([a] :- r)
  rJoin f = xpure (arg (arg (:-)) (++)) $ \(xs :- t) -> do as <- Just xs; let (s1, s2) = f as in Just (s1 :- s2 :- t);
  -- exponent part
  e = opt (rJoin (\s -> (s, "")) . (rCons . oneOf "eE") . (rCons . oneOf "+-") . rList1 digit) . push ""
  -- fractional part
  f = (rCons . char '.') . (rList1 digit <> push "0")
  fe = rJoin (break (`elem` "eE")) . f . e
  -- integer part
  i = opt (rCons . oneOf "+-") . (rList1 digit <> push "0")
  -- all together
  ife = rJoin (break (== '.')) . i . fe

  readFloat :: StringBoomerang (String :- r) (Value :- r)
  readFloat = xpure (arg (:-) (\n@(s:f) -> case s of
                                            '+'       -> FloatValue . read $ f
                                            otherwise -> FloatValue . read $ n))
                    floatSerizlier

  floatSerizlier :: (Value :- r) -> Maybe (String :- r)
  floatSerizlier (FloatValue f :- r) = Just (show f :- r)
  floatSerizlier _                   = Nothing

number :: StringBoomerang r (Value :- r)
number = octalNumber <> decNumber <> hexNumber <> floatNumber

true :: StringBoomerang r (Value :- r)
true = push (BooleanValue True) . (lit "true" <> lit "yes" <> lit "on")

false :: StringBoomerang r (Value :- r)
false = push (BooleanValue False) . (lit "false" <> lit "no" <> lit "off")

-- Dangerous parser - it scans all results of given parser for longest one
parse1Partial :: Boomerang StringError String () (t :- ()) ->
                 String ->
                 MajorMinorPos ->
                 Either [StringError] ((t, String), MajorMinorPos)
parse1Partial parser input position =
  let rawResult = runParser (prs parser) input position
      results = [either Left (\((f, t), p) -> Right ((f (), t), p)) r | r <- rawResult]
  in case maximumByMay (compare `on` snd) [r | (Right r) <- results] of
           (Just ((u :- (), t), p)) -> Right ((u, t), p)
           _             -> Left $ bestErrors [ e | Left e <- results ]
 where
  -- version from Safe.Foldable returns last maximum from list
  -- but we need first the first one
  maximumByMay c = maximumByMay' c Nothing

  maximumByMay' :: (a -> a -> Ordering) -> Maybe a -> [a] -> Maybe a
  maximumByMay' _ r []     = r
  maximumByMay' c Nothing (x:xs) = maximumByMay' c (Just x) xs
  maximumByMay' c (Just r) (x:xs) =
    if c x r == GT
      then maximumByMay' c (Just x) xs
      else maximumByMay' c (Just r) xs

delete :: StringBoomerang () (a :- ()) -> StringBoomerang r r
delete b =
  Boomerang pf sf
 where
  pf =
    Parser $ \tok pos ->
      case parse1Partial b tok pos of
        Right ((a, tok'), pos') -> [Right ((id, tok'), pos')]
        Left e -> [Left . condenseErrors $ e]
  sf r = [(id, r)]

-- currently we are droping all comments
comment :: StringBoomerang r r
comment =
  delete (manyl whiteSpace . rCons . char '#' . rList1 (noneOf "\n")) . eol

identifier = unquotedString
openBracket = char '<'
closeBracket = char '>'

unquote :: StringBoomerang (QuotedString :- r) (Value :- r)
unquote = xpure (arg (:-) u) q
 where
  u = StringValue . concatMap u'
  u' (Escaped s) = s
  u' (Literal s) = s

  q (StringValue s :- r) = Just (q' s :- r)
  q _                    = Nothing
  q' []       = []
  q' ('\\':r) = qCons (Escaped "\\") (q' r)
  q' ('"':r)  = qCons (Escaped "\"") (q' r)
  q' (c:r)    = qCons (Literal [c]) (q' r)

  qCons :: QuotedStringPart -> QuotedString -> QuotedString
  qCons (Literal [c]) (Literal t : s) = Literal (c:t) : s
  qCons (Literal h) (Literal t : s) = Literal (h ++ t) : s
  qCons (Escaped [c]) (Escaped t : s) = Escaped (c:t) : s
  qCons (Escaped h) (Escaped t : s) = Escaped (h ++ t) : s
  qCons e s = e : s

string = unquotedString <> unquote . quotedString

-- XXX: Still missing parsers
-- PORT (6(5(5(3[0-5]|[0-2][0-9])|[0-4][0-9][0-9])|[0-4][0-9][0-9][0-9])|[1-5][0-9][0-9][0-9][0-9]|[1-9][0-9]?[0-9]?[0-9]?)
-- 
-- IP_BYTE (2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])
-- IPV4_ADDR {IP_BYTE}\.{IP_BYTE}\.{IP_BYTE}\.{IP_BYTE}(:{PORT})?
-- 
--
-- /* IPv6 address according to http://www.ietf.org/rfc/rfc2373.txt
--  * This supports embedded IPv4 addresses as well but does not strictly check
--  * for the right prefix (::0:<v4> or ::FFFF:<v4>) because there are too many
--  * ways to correctly represent the zero bytes. It's up to the user to check
--  * for valid addresses. */
-- HEX16 ([0-9A-Fa-f]{1,4})
-- V6_PART ({HEX16}:{HEX16}|{IPV4_ADDR})
-- IPV6_BASE ({HEX16}:){6}{V6_PART}|::({HEX16}:){5}{V6_PART}|({HEX16})?::({HEX16}:){4}{V6_PART}|(({HEX16}:){0,1}{HEX16})?::({HEX16}:){3}{V6_PART}|(({HEX16}:){0,2}{HEX16})?::({HEX16}:){2}{V6_PART}|(({HEX16}:){0,3}{HEX16})?::{HEX16}:{V6_PART}|(({HEX16}:){0,4}{HEX16})?::{V6_PART}|(({HEX16}:){0,5}{HEX16})?::{HEX16}|(({HEX16}:){0,6}{HEX16})?::
-- IPV6_ADDR ({IPV6_BASE})|(\[{IPV6_BASE}\](:{PORT})?)

data Value = BooleanValue Bool
           | FloatValue Float
           | IntValue Int
           | StringValue String
  deriving (Show, Eq)

argument :: StringBoomerang r (Value :- r)
argument = number <> true <> false <> string

argumentList :: StringBoomerang r ([Value] :- r)
argumentList = rList (somel whiteSpace . argument)

type Label = String
data Option = Option Label [Value] | Section Label [Value] [Option]
  deriving (Eq, Show)

optionLabel :: Option -> Label
optionLabel (Option l _) = l
optionLabel (Section l _ _) = l

option :: StringBoomerang r (Option :- r)
option =
  Boomerang pf sf
 where
  pf =
    Parser $ \tok pos ->
      case parse1Partial (rPair . section' . lit "</" . identifier . lit ">") tok pos of
        Right (((opt, StringValue closingTag), tok'), pos') ->
          let label = optionLabel opt
          in if label == closingTag
              then [Right ((\r -> opt :- r, tok'), pos')]
              else mkParserError pos'
                [ Expect label
                , Message
                    ("Closing tag (" ++
                     closingTag ++
                     ") should be the same as openning one (" ++
                     label ++
                     ")")
                ]
        Left e  -> runParser (prs option') tok pos
  sf s@(Option i args :- r) = ser option' s
  sf (s@(Section i args opts :- r))  = [(fmap (++ ("</" ++ i ++ ">")) tok2tok, r) |(tok2tok, r) <- ser section' s]

  section' = assembleOption . lit "<" . identifier . argumentList . manyl whiteSpace . lit ">" .
             eolOrComment . options

  option' :: StringBoomerang r (Option :- r)
  option' =
    assembleOption . identifier . argumentList . push []

  assembleOption :: StringBoomerang (Value :- [Value] :- [Option] :- r) (Option :- r)
  assembleOption =
    xpure (arg (arg (arg (:-))) (\(StringValue i) as os -> if null os then Option i as else Section i as os))
           optionSerializer

   where
    optionSerializer :: (Option :- r) -> Maybe (Value :- [Value] :- [Option] :- r)
    optionSerializer (Option i as :- r) = Just (StringValue i :- as :- [] :- r)
    optionSerializer (Section i as os :- r) = Just (StringValue i :- as :- os :- r)


options :: Boomerang StringError String r ([Option] :- r)
options = rList1 ((manyl whiteSpace . option) . somel eolOrComment) <> manyr eolOrComment . push []

-- data Globals =
--        Globals
--          { autoLoadPlugin :: Optional Bool
--          , baseDir :: Optional FilePath
--          , hostname :: Optional String
--          , includePath :: Optional Include
--          , interval :: Optional Seconds
--          , pidFile :: Optional FilePath
--          , pluginDir :: Optional FilePath
--          , readThreads :: Optional Int
--          , typesDB :: Optional [FilePath]
--          , timeout :: Optional Iterations
--          , writeThreads :: Optional Int
--          , writeQueueLimitHigh :: Optional Int
--          , writeQueueLimitLow :: Optional Int
--          }
--   -- PostCacheChain ChainName
--   -- FQDNLookup true|false
--   -- PreCacheChain ChainName
--   deriving Show
