{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding ((.), id)
import           Control.Category ((.))
import           System.EtCetera.Interfaces (Asquisition(..), Protocol(..), rEol, rIface, rRawIfaceOption,
                                             RawIfaceOption(..), Stanza(..))
import           Text.Boomerang.Combinators (rList1)
import           Text.Boomerang.String (unparseString, parseString)
import           Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $
  describe "EtCetera.Interfaces boomerang" $ do
    it "rRawIfaceOption parses single, simple iface option correctly" $
      parseString rRawIfaceOption " address 192.168.1.100" `shouldBe`
        (Right . RawIfaceOption "address" $ ["192.168.1.100"])
    it "rRawIfaceOption parses multiple options separated by newline" $
      parseString (rList1 (rEol . rRawIfaceOption))
                  "\n address 192.168.1.100\n dns-servers 8.8.8.8"
        `shouldBe`
          Right [ RawIfaceOption "address" ["192.168.1.100"]
                , RawIfaceOption "dns-servers" ["8.8.8.8"]
                ]
    it "rIface parses iface stanza with multiple options" $
      parseString rIface ("iface eth0 inet dhcp\n" ++
                          "  address 192.168.1.100\n" ++
                          "  dns-servers 8.8.8.8 4.4.4.4")
        `shouldBe`
          Right (Iface "eth0" Inet DHCP [ RawIfaceOption "address" ["192.168.1.100"]
                                        , RawIfaceOption "dns-servers" ["8.8.8.8", "4.4.4.4"]])
    -- it "rIface parses iface stanza with multiple options" $
    --   print $ parseString rIface ("iface eth0 inet \\\ndhcp\n" ++
    --                                   "  address 192.168.1.100\n" ++
    --                                   "  dns-servers 8.8.8.8 4.4.4.4")
