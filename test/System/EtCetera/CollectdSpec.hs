module System.EtCetera.CollectdSpec where

import           Control.Category ((.))
import           Prelude hiding ((.), id)
import           System.EtCetera.Collectd (number, floatNumber, quotedString,
                                           QuotedStringPart(..), Value(..))
import           Text.Boomerang.String (unparseString, parseString)
import           Test.Hspec (describe, it, shouldBe, Spec)

suite :: Spec
suite = do
  describe "EtCetera.Collectd.quotedString boomerang" $ do
    it "escapes correctly characters" $
      unparseString quotedString [Literal "smells like", Escaped '"', Literal "Teen spirit", Escaped '"']
        `shouldBe` (Just "\"smells like\\\"Teen spirit\\\"\"")
  -- describe "EtCetera.Collectd.floatNumber boomerang" $ do
  --   it "parses simple float number" $
  --     parseString floatNumber "3.14" `shouldBe`
  --       (Right . FloatValue $ 3.14)
  describe "EtCetera.Collectd.number boomerang" $ do
    it "parses simple octal number" $
      parseString number "020" `shouldBe`
        (Right . IntValue $ 16)
    it "parses simple hex number" $
      parseString number "0x20" `shouldBe`
        (Right . IntValue $ 32)
    it "parses float number without exponent part" $
      parseString number "3.14" `shouldBe`
        (Right . FloatValue $ 3.14)
    it "parses float number with exponent part" $
      parseString number "3.14e+0" `shouldBe`
        (Right . FloatValue $ 3.14)
    it "parses simple negative float number" $
      parseString number "-3.14" `shouldBe`
        (Right . FloatValue $ -3.14)
    it "parses negative float number without integer part" $
      parseString number "-.14" `shouldBe`
        (Right . FloatValue $ -0.14)
    it "parses float number with exponent part and plus prefix" $
      parseString number "+.314e+1" `shouldBe`
        (Right . FloatValue $ 3.14)
    it "parses simple integer number" $
      parseString number "20" `shouldBe`
        (Right . IntValue $ 20)
