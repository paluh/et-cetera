{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module System.EtCetera.Interfaces where

import           Prelude hiding ((.), id)
import           Control.Category ((.))
import           Data.Monoid ((<>))
import           Data.Char (toLower, toUpper)
import           Data.Maybe (fromMaybe)
import           Data.IP (IP(..), toIPv4, fromIPv4)
import           Text.Boomerang (arg, Boomerang, manyl, opt, somel, xpure)
import           Text.Boomerang.Combinators (rCons, rNil, rList, rList1, rListSep)
import           Text.Boomerang.String (alpha, anyChar, char, digit, int, lit, StringBoomerang,
                                        unparseString, parseString)
import           Text.Boomerang.HStack ((:-)(..))

data Asquisition = DHCP | Manual | PPP | Static
  deriving (Eq, Show)

data Protocol = Inet | Inet6
  deriving (Eq, Read, Show)

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c:cs

rProtocol :: forall tok e r. Boomerang e tok (String :- r) (Protocol :- r)
rProtocol =
  xpure (arg (:-) (read . capitalize))
        (Just . arg (:-) (map toLower . show))

word :: String -> StringBoomerang r (String :- r)
word []     = rNil
word (x:xs) = rCons . char x . word xs

label :: StringBoomerang r (String :- r)
label = rCons . alpha . value

value :: StringBoomerang r (String :- r)
value = rList1 (digit <> alpha <> char '.' <> char '-')

spaces :: StringBoomerang r r
spaces = somel (lit " " <> lit "\t" <> lit "\\\n")

indentation :: StringBoomerang r r
indentation = somel (lit " " <> lit "\t") . opt spaces

rEol :: StringBoomerang r r
rEol = lit "\n"

empty :: StringBoomerang r r
empty = opt spaces . rEol <> spaces

rProtocolB :: StringBoomerang r (Protocol :- r)
rProtocolB =
    rProtocol . (word "inet" <> word "inet6")

rAsquisition :: forall tok e r. Boomerang e tok (String :- r) (Asquisition :- r)
rAsquisition =
  xpure (arg (:-) r)
        (Just . arg (:-) (map toLower . show))
 where
  r "dhcp"   = DHCP
  r "manual" = Manual
  r "ppp"    = PPP
  r "static" = Static
  r _        = error "rAsquisition -> parser should pass only correct strings"

rAsquisitionB :: StringBoomerang r (Asquisition :- r)
rAsquisitionB = rAsquisition . (word "dhcp" <> word "manual" <> word "ppp" <> word "static")

type Name = String
data Stanza =
    Iface Name Protocol Asquisition [RawIfaceOption] |
    Auto Name
  deriving (Eq, Show)

rInterface :: forall tok e r. Boomerang e tok (Name :- Protocol :- Asquisition :- [RawIfaceOption] :- r) (Stanza :- r)
rInterface =
  xpure
    (arg (arg (arg (arg (:-)))) Iface)
    (\(Iface n p a o :- r) -> Just (n :- p :- a :- o :- r))

data RawIfaceOption = RawIfaceOption String [String]
  deriving (Eq, Show)

rRawIfaceOption :: StringBoomerang r (RawIfaceOption :- r)
rRawIfaceOption =
  rRawIfaceOption' . indentation . label . rList1 (spaces . value)
 where
  rRawIfaceOption' :: StringBoomerang (String :- [String] :- r) (RawIfaceOption :- r)
  rRawIfaceOption' =
    xpure
      (arg (arg (:-)) RawIfaceOption)
      (\(RawIfaceOption l as :- r) -> Just (l :- as :- r))


rIface :: StringBoomerang r (Stanza :- r)
rIface =
  manyl empty .
  rInterface .
  lit "iface" .
  spaces .
  rList1 anyChar .
  spaces .
  rProtocolB .
  spaces .
  rAsquisitionB .
  rList (rEol . rRawIfaceOption)

rIp4 :: forall tok e r. Boomerang e tok (Int :- Int :- Int :- Int :- r) (IP :- r)
rIp4 = xpure
          (arg (arg (arg (arg (:-)))) (\q1 q2 q3 q4 -> IPv4 . toIPv4 $ [q1, q2, q3, q4]))
          (\ip -> case ip of
                  (IPv4 i :- r) -> Just . (\[q1, q2, q3, q4] -> (q1 :- q2 :- q3 :- q4 :- r)) . fromIPv4 $ i
                  _ -> Nothing)

ipB :: StringBoomerang r (IP :- r)
ipB =
  rIp4 . int . "." . int . "." . int . "." . int

main :: IO ()
main = do
  print $ unparseString ipB (IPv4 . toIPv4  $ [192, 168, 1, 100])
  print $ parseString (rListSep value spaces) "address 192.168.1.100"
