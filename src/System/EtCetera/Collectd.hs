{-# LANGUAGE TypeOperators #-}
module System.EtCetera.Collectd where

-- Based directly on:
--  https://github.com/collectd/collectd/blob/master/src/liboconfig/parser.y
--  https://github.com/collectd/collectd/blob/master/src/liboconfig/scanner.l

import           Control.Category ((.))
import           Data.Char (ord)
import           Data.Monoid ((<>))
import           Numeric (showHex, showOct)
import           Prelude hiding ((.), id)
import           Text.Boomerang.Combinators (manyl, opt, push, rCons, rList, rList1, rNil, somel)
import           Text.Boomerang.HStack (arg, (:-)(..))
import           Text.Boomerang.Prim (Boomerang, xpure)
import           Text.Boomerang.String (alpha, anyChar, char, digit, lit, satisfy, StringBoomerang)

type Name = String
type Arg = String

data Var = Var Name [Arg] [Var]

oneOf :: String -> StringBoomerang r (Char :- r)
oneOf l = satisfy (`elem` l)

notAnyOf :: String -> StringBoomerang r (Char :- r)
notAnyOf l = satisfy (not . (`elem` l))

digitInRange :: Int -> Int -> StringBoomerang r (Char :- r)
digitInRange s e = oneOf . concatMap show $ [s..e]

alphaInRange :: Char -> Char -> StringBoomerang r (Char :- r)
alphaInRange s e = oneOf . concatMap show $ [ord s..ord e]

whiteSpace :: StringBoomerang r (Char :- r)
whiteSpace = oneOf " \t\b"

nonWhiteSpace :: StringBoomerang r (Char :- r)
nonWhiteSpace = notAnyOf "\ \t\b"

eol :: StringBoomerang r r
eol = lit "\r\n" <> lit "\n"

data QuotedStringPart = Escaped Char | Literal String
type QuotedString = [QuotedStringPart]

qCons :: QuotedStringPart -> QuotedString -> QuotedString
qCons (Literal [c]) (Literal t : s) = Literal (c:t) : s
qCons (Literal h) (Literal t : s) = Literal (h ++ t) : s
qCons e s = e : s

quotedString :: StringBoomerang r (QuotedString :- r)
quotedString = lit "\"" . rList1 (literal . rList1 (notAnyOf "\\\"") <> escaped . rCons . char '\\' . rCons . anyChar . rNil) . lit "\""
 where
  escaped = xpure
          (arg (:-) (\[e, c] -> Escaped c))
          (\e -> case e of
                  (Escaped c :- r) -> Just (['\\', c] :- r)
                  _ -> Nothing)
  literal = xpure
          (arg (:-) Literal)
          (\e -> case e of
                  (Literal s :- r) -> Just (s :- r)
                  _ -> Nothing)

unquotedString :: StringBoomerang r (Value :- r)
unquotedString = xpure (arg (:-) StringValue) (\(StringValue s :- r)-> Just (s :- r)) . rList1 (alpha <> digit)

-- HEX_NUMBER 0[xX][0-9a-fA-F]+
hexNumber :: StringBoomerang r (Value :- r)
hexNumber =
  hex . (rCons . char '0') . (rCons  . oneOf "xX") .
    rList1 (digit <> alphaInRange 'a' 'f' <> alphaInRange 'A' 'F')
 where
  hex = xpure (arg (:-) (IntValue . read))
                    (\(IntValue x :- r) -> Just (("0x" ++ showHex x "") :- r))

-- OCT_NUMBER 0[0-7]+
octalNumber :: StringBoomerang r (Value :- r)
octalNumber =
  octal . (rCons . char '0' . rCons . push 'o') . rList1  (digitInRange 0 7)
 where
  octal = xpure (arg (:-) (IntValue . read))
                    (\(IntValue x :- r) -> Just (("0x" ++ showOct x "") :- r))

-- DEC_NUMBER [\+\-]?[0-9]+
decNumber :: StringBoomerang r (Value :- r)
decNumber =
  int . opt (rCons . oneOf "+-") . rList1 digit
 where
  int = xpure (arg (:-) (\x -> case x of
                                    ('+':r)   -> IntValue (read r)
                                    otherwise -> IntValue (read x)))
                  (\(IntValue x :- r) -> Just (show x :- r))

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
                    (\(FloatValue f :- r) -> Just (show f :- r))

number :: StringBoomerang r (Value :- r)
number = octalNumber <> decNumber <> hexNumber <> floatNumber

true :: StringBoomerang r (Value :- r)
true = push (BooleanValue True) . (lit "true" <> lit "yes" <> lit "on")

false :: StringBoomerang r (Value :- r)
false = push (BooleanValue False) . (lit "false" <> lit "no" <> lit "off")

comment :: StringBoomerang r (String :- r)
comment = rCons . char '#' . rList anyChar . eol

identifier = unquotedString
openBracket = char '<'
closeBracket = char '>'

unquote :: StringBoomerang (QuotedString :- r) (Value :- r)
unquote = xpure (arg (:-) u) q
 where
  u = StringValue . concatMap u'
  u' (Escaped c) = ['\\', c]
  u' (Literal s) = s

  q (StringValue s :- r) = Just (q' s :- r)
  q' ('\\':r) = qCons (Escaped '\\') (q' r)
  q' ('"':r) = qCons (Escaped '"') (q' r)
  q' (c:r) = qCons (Literal [c]) (q' r)


string = unquotedString <> unquote . quotedString

data Value = BooleanValue Bool
           | FloatValue Float
           | IntValue Int
           | StringValue String
  deriving (Show, Eq)

argument :: StringBoomerang r (Value :- r)
argument = string <> true <> false

argumentList :: StringBoomerang r ([Value] :- r)
argumentList = rCons . argument . argumentList <> rNil

type Label = String
data Option = Option Label [Value]

option :: StringBoomerang r (Option :- r)
option =
  option' . identifier . argumentList . eol
 where
  option' :: StringBoomerang (Value :- [Value] :- r) (Option :- r)
  option' = xpure (arg (arg (:-)) (\(StringValue i) as -> Option i as))
                  (\(Option i as :- r) -> Just (StringValue i :- as :- r))
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