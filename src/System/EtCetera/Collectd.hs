{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module System.EtCetera.Collectd where

-- Based directly on:
--  https://github.com/collectd/collectd/blob/master/src/liboconfig/parser.y
--  https://github.com/collectd/collectd/blob/master/src/liboconfig/scanner.l

import           Control.Applicative (Applicative(..), (<$>), (<*>))
import           Control.Arrow (first)
import           Control.Category ((.), id)
import           Data.Char (ord)
import           Data.Function (on)
import           Data.List (intersperse)
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import           Data.Optional (Optional(..))
import           Data.Ord (compare, Ordering(..))
import           Debug.Trace (trace)
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
data ConfigOption = ConfigOption Label [Value] | ConfigSection Label [Value] [ConfigOption]
  deriving (Eq, Show)

optionLabel :: ConfigOption -> Label
optionLabel (ConfigOption l _) = l
optionLabel (ConfigSection l _ _) = l

-- XXX: please, refactor this mess
option :: String -> StringBoomerang r (ConfigOption :- r)
option indent =
  Boomerang pf sf
 where
  pf =
    Parser $ \tok pos ->
      case parse1Partial (rPair . section' indent . lit "</" . identifier . lit ">") tok pos of
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
  -- TODO: analyze this proposition again
  -- sf (s@(ConfigSection i args [] :- r))  = ser option' s
  sf s@(ConfigOption i args :- r) = ser option' s
  sf (s@(ConfigSection i args opts :- r))  =
    ser section'' s
   where
    section'' = assembleConfigOption . lit "<" . identifier . argumentList . manyl whiteSpace . lit ">" .
                options' ('\t':indent) . lit "\n" . (lit indent <> manyl whiteSpace) . lit ("</" ++ i ++ ">")

  option' :: StringBoomerang r (ConfigOption :- r)
  option' =
    assembleConfigOption . identifier . argumentList . push []

  section' indent = assembleConfigOption . lit "<" . identifier . argumentList . manyl whiteSpace . lit ">" .  options' indent

  options' :: String -> StringBoomerang r ([ConfigOption] :- r)
  options' indent = rList (somel eolOrComment . (lit indent <> manyl whiteSpace) . option indent) . manyl eolOrComment


assembleConfigOption :: StringBoomerang (Value :- [Value] :- [ConfigOption] :- r) (ConfigOption :- r)
assembleConfigOption =
  xpure (arg (arg (arg (:-))) (\(StringValue i) as os -> if null os then ConfigOption i as else ConfigSection i as os))
         optionSerializer
 where
  optionSerializer :: (ConfigOption :- r) -> Maybe (Value :- [Value] :- [ConfigOption] :- r)
  optionSerializer (ConfigOption i as :- r) = Just (StringValue i :- as :- [] :- r)
  optionSerializer (ConfigSection i as os :- r) = Just (StringValue i :- as :- os :- r)

data Option = Option Label [Value] [Option]
  deriving (Eq, Show)

convertOpt :: StringBoomerang (ConfigOption :- r) (Option :- r)
convertOpt =
  xpure (arg (:-) toOpt) (\(o :- r) ->  (Just (fromOpt o :- r)))
 where
  toOpt (ConfigOption l vs) = Option l vs []
  toOpt (ConfigSection l vs os) = Option l vs (map toOpt os)

  fromOpt (Option l vs []) = ConfigOption l vs
  fromOpt (Option l vs os) = ConfigSection l vs (map fromOpt os)

options :: String -> StringBoomerang r ([Option] :- r)
options indent = (rCons . manyl whiteSpace . convertOpt . option indent <> id) .
                  rList (somel eolOrComment . manyl whiteSpace . convertOpt . option indent) .
                  manyl eolOrComment

data Globals = Globals { autoLoadPlugin :: Bool, baseDir :: String }
  deriving (Eq, Show)


-- inspired by: http://www.paolocapriotti.com/blog/2012/04/27/applicative-option-parser/
data OptionParser v = OptionParser { optParser :: Maybe Option -> Maybe v }

instance Functor OptionParser where
 fmap f (OptionParser p) = OptionParser (fmap (fmap f) p)

data OptParser a where
  NilP :: a -> OptParser a
  ConsP :: (Maybe Option -> Maybe (b -> a)) -> OptParser b -> OptParser a

instance Functor OptParser where
  fmap f (NilP a) = NilP (f a)
  fmap f (ConsP opt pb) = ConsP (fmap (fmap (fmap f)) opt) pb

instance Applicative OptParser where
  pure = NilP
  (<*>) (NilP a2b) (pa) = a2b <$> pa
  (<*>) (ConsP optb2a2c pb) pa = ConsP (fmap (fmap uncurry) optb2a2c) ((,) <$> pb <*> pa)

run :: OptParser a -> [Option] -> Maybe (a, [Option])
run (NilP a) os  = Just (a, os)
run (ConsP opt pb) [] = do
  (b2a, os') <- fmap (flip (,) []) (opt Nothing)
  run (fmap b2a pb) os'
run p (o:os) =
  case step p o os of
    Nothing -> Nothing
    Just (p', os') -> run p' os'

step :: OptParser a -> Option -> [Option] -> Maybe (OptParser a, [Option])
step p@(NilP _) _ [] = Just (p, [])
step (NilP _) _ _    = Nothing
step (ConsP o2b2a pb) option@(Option n' _ _) os =
  case o2b2a (Just option) of
    Nothing -> do (pb', os') <- step pb option os
                  return (ConsP o2b2a pb', os')
    Just b2a -> Just (fmap b2a pb, os)

p :: OptionParser v -> OptParser v
p (OptionParser v) = ConsP (\o -> do r <- v o; return . const $ r) (NilP ())

boolOptPrs :: Label -> OptionParser Bool
boolOptPrs l =
  OptionParser c
 where
  c (Just (Option l [BooleanValue b] [])) = Just b
  c _                                     = Nothing

strOptPrs :: Label -> OptionParser String
strOptPrs l =
  OptionParser c
 where
  c (Just (Option _ [StringValue s] [])) = Just s
  c _                                    = Nothing


globalsParser = Globals <$> (p . boolOptPrs $ "autoLoadPlugin") <*> (p . strOptPrs $ "baseDir")

globals :: StringBoomerang ([Option] :- r) (Maybe Globals :- r)
globals =
  xpure (arg (:-) toGlobals) undefined
 where
  --  run :: OptParser a -> [Option] -> Maybe (a, [Option])
  toGlobals opts = fst <$> run globalsParser opts


-- ([Option] -> (a, [Option]))

-- data Globals =
--        Globals
--          { autoLoadPlugin :: ConfigOptional Bool
--          , baseDir :: ConfigOptional FilePath
--          , hostname :: ConfigOptional String
--          -- , includePath :: ConfigOptional Include
--          , interval :: ConfigOptional Int
--          , pidFile :: ConfigOptional FilePath
--          , pluginDir :: ConfigOptional FilePath
--          , readThreads :: ConfigOptional Int
--          , typesDB :: ConfigOptional [FilePath]
--          -- , timeout :: ConfigOptional Iterations
--          , writeThreads :: ConfigOptional Int
--          , writeQueueLimitHigh :: ConfigOptional Int
--          , writeQueueLimitLow :: ConfigOptional Int
--          }
--   -- PostCacheChain ChainName
--   -- FQDNLookup true|false
--   -- PreCacheChain ChainName
--   deriving Show

-- data ConfigOptionParser = {


