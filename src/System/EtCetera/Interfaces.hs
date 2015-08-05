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
  deriving (Show)

data Protocol = Inet | Inet6
  deriving (Read, Show)

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

eol :: StringBoomerang r r
eol = lit "\n"

empty :: StringBoomerang r r
empty = opt spaces . eol <> spaces

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
    Iface Name Protocol Asquisition [RawOption] |
    Auto Name
  deriving (Show)

rInterface :: forall tok e r. Boomerang e tok (Name :- Protocol :- Asquisition :- [RawOption] :- r) (Stanza :- r)
rInterface =
  xpure
    (arg (arg (arg (arg (:-)))) Iface)
    (\(Iface n p a o :- r) -> Just (n :- p :- a :- o :- r))

data RawOption = RawOption String [String]
  deriving (Show)

rRawOption :: StringBoomerang (String :- [String] :- r) (RawOption :- r)
rRawOption =
  xpure
    (arg (arg (:-)) RawOption)
    (\(RawOption l as :- r) -> Just (l :- as :- r))

rRawOptionB :: StringBoomerang r (RawOption :- r)
rRawOptionB =
  rRawOption . spaces . label . rList1 (spaces . value)

interfaceB :: StringBoomerang r (Stanza :- r)
interfaceB =
  manyl empty .
  rInterface .
  lit "iface" .
  spaces .
  rList1 anyChar .
  spaces .
  rProtocolB .
  spaces .
  rAsquisitionB .
  rList (eol . rRawOptionB)

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
  print $ parseString rRawOptionB " address 192.168.1.100"
  print $ parseString (rListSep rRawOptionB eol) " address 192.168.1.100\n dns-servers 8.8.8.8"
  print $ parseString (rListSep rRawOptionB eol) " address 192.168.1.100\n dns-servers 8.8.8.8 4.4.4.4"
  print $ parseString interfaceB ("iface eth0 inet dhcp\n" ++
                                  "  address 192.168.1.100\n" ++
                                  "  dns-servers 8.8.8.8 4.4.4.4")
  -- test \\n pattern
  print $ parseString interfaceB ("iface eth0 inet \\\ndhcp\n" ++
                                  "  address 192.168.1.100\n" ++
                                  "  dns-servers 8.8.8.8 4.4.4.4")
  print $ unparseString interfaceB (Iface "eth0" Inet DHCP [])
  putStrLn . fromMaybe "" $ unparseString interfaceB
                              (Iface "eth0" Inet DHCP [RawOption "address" ["192.168.1.1"]])
  putStrLn . fromMaybe "" $ unparseString (rListSep (char 'a') " ") "aaa"
  putStrLn . fromMaybe "" $ unparseString interfaceB
                              (Iface "eth0" Inet DHCP
                                [ RawOption "address" ["192.168.1.101"]
                                , RawOption "dns-servers" ["8.8.8.8", "4.4.4.4"]
                                , RawOption "network" ["192.168.1.100"]])
