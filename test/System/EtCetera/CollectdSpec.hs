{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.CollectdSpec where

import           Control.Category ((.), id)
import           Data.Maybe (listToMaybe)
import           Prelude hiding ((.), id)
import           System.EtCetera.Collectd (argumentList, section, number, floatNumber, option, Option(..), quotedString,
                                           string, QuotedString, QuotedStringPart(..), Value(..))
import           Text.Boomerang.Combinators (rList1)
import           Text.Boomerang.String (unparseString, parseString, StringError)
import           Test.Hspec (describe, it, shouldBe, Spec)

suite :: Spec
suite = do
  describe "EtCetera.Collectd.quotedString boomerang" $ do
    it "parses escaped character" $
      parseString quotedString "\"\\\"\"" `shouldBe`
        Right [Escaped "\""]
    it "parses long list of quoted strings without escapes" $
      parseString (rList1 quotedString) "\"first\"\"second\"\"third\"\"fourth\"\"fifth\"" `shouldBe`
        Right [[Literal "first"], [Literal "second"], [Literal "third"], [Literal "fourth"], [Literal "fifth"]]
    it "parses literal string which ends with escaped sequence" $
      parseString (rList1 quotedString) "\"literalwithdirtyend\\\"\"" `shouldBe`
        Right [[Literal "literalwithdirtyend", Escaped "\""]]
    it "parses literal strings separated by escaped sequence" $
      parseString quotedString "\"smells like\\\"Teen spirit\"" `shouldBe`
        Right [Literal "smells like", Escaped "\"", Literal "Teen spirit"]
    it "parses literal strings separated by escaped sequences" $
      parseString (rList1 quotedString) "\"smells like\\\"teen\\\"spirit\"" `shouldBe`
        Right [[Literal "smells like", Escaped "\"", Literal "teen", Escaped "\"", Literal "spirit"]]
    it "prints simple string correctly" $
      unparseString quotedString [Literal "smells like"]
        `shouldBe` Just "\"smells like\""
    it "prints escaped sequence correctly" $
      unparseString quotedString [Escaped "\""]
        `shouldBe` Just "\"\\\"\""
    it "prints escaped sequences correctly" $
      unparseString quotedString [Escaped "\"\""]
        `shouldBe` Just "\"\\\"\\\"\""
    it "prints literal which ends with escape sequence" $
      unparseString quotedString [Literal "smells like", Escaped "\""]
        `shouldBe` Just "\"smells like\\\"\""
    it "prints literal which ends with escape sequences" $
      unparseString quotedString [Literal "smells like", Escaped "\"\""]
        `shouldBe` Just "\"smells like\\\"\\\"\""
    it "prints literal strings separated by escape sequence" $
      unparseString quotedString [Literal "smells like", Escaped "\"", Literal "Teen spirit"]
        `shouldBe` Just "\"smells like\\\"Teen spirit\""
    it "prints literal strings separated by escape sequences" $
      unparseString quotedString [Literal "smells like", Escaped "\"\"", Literal "Teen spirit"]
        `shouldBe` Just "\"smells like\\\"\\\"Teen spirit\""
    it "parses literal strings separated by escape sequences" $
      parseString quotedString "\"smells like\\\"\\\"Teen spirit\"" `shouldBe`
        Right [Literal "smells like", Escaped "\"\"", Literal "Teen spirit"]
    it "prints literal strings separated by escape sequences and escape sequence at the end" $
      unparseString quotedString [Literal "smells like", Escaped "\"\"", Literal "Teen spirit", Escaped "\""]
        `shouldBe` Just "\"smells like\\\"\\\"Teen spirit\\\"\""
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
  describe "EtCetera.Collectd.argument boomerang" $ do
    it "parses single quoted path" $
      parseString argumentList " \"/etc/collectd.d/*\"" `shouldBe`
        Right [StringValue "/etc/collectd.d/*"]
    it "parses quoted words" $
     parseString argumentList " \"host\" \"localhost\"" `shouldBe`
       Right [StringValue "host", StringValue "localhost"]
    it "parses quoted paths" $
     parseString argumentList " \"/etc/collectd.d\" \"*.conf\"" `shouldBe`
       Right [StringValue "/etc/collectd.d", StringValue "*.conf"]
  describe "EtCetera.Collectd.option boomerang" $ do
    it "parses option with single argument" $
      parseString option "LoadPlugin cpu" `shouldBe`
        (Right . Option "LoadPlugin" [StringValue "cpu"] $ [])
    it "parses simple option with single quoted argument" $
      parseString option "LoadPlugin \"cpu\"" `shouldBe`
        (Right . Option "LoadPlugin" [StringValue "cpu"] $ [])
    it "parses option with multiple unquoted arguments" $
      parseString option "DriverOption host localhost" `shouldBe`
        (Right . Option "DriverOption" [StringValue "host", StringValue "localhost"] $ [])
    it "parses option with multiple quoted simple arguments" $
      parseString option "DriverOption \"host\" \"localhost\"" `shouldBe`
        (Right . Option "DriverOption" [StringValue "host", StringValue "localhost"] $ [])
    it "parses option with multiple quoted paths" $
      parseString option "Include \"/etc/collectd.d/*\" \"localhost\"" `shouldBe`
        (Right . Option "Include" [StringValue "/etc/collectd.d/*", StringValue "localhost"] $ [])
  describe "EtCetera.Collectd.section boomerang" $ do
    it "parses empty section" $
      parseString section "<Plugin>\n</Plugin>" `shouldBe`
        (Right . Option "Plugin" [] $ [])
    it "parses section with arguments" $
      parseString section "<Plugin arg1 1 2>\n</Plugin>" `shouldBe`
        (Right . Option "Plugin" [StringValue "arg1", IntValue 1, IntValue 2] $ [])
    it "parses section with arguments and children" $
      parseString section "<Plugin arg1 1 2>\nChild1 2.8\n</Plugin>" `shouldBe`
        (Right . Option "Plugin" [StringValue "arg1", IntValue 1, IntValue 2] $ [Option "Child1" [FloatValue 2.8] []])


