module System.EtCetera.InternalSpec where

import           Data.Either (isLeft)
import           System.EtCetera.Internal.Boomerangs (quotedString, quotedString', QuotedStringPart(..))
import           Text.Boomerang.Combinators (push, rList1)
import           Text.Boomerang.String (unparseString, parseString)
import           Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)

suite :: Spec
suite = -- do
  describe "System.EtCetera.Internal.Boomerangs.quotedString'" $ do
    it "parses escaped character" $
      parseString quotedString' "\"\\\"\"" `shouldBe`
        Right [Escaped "\""]
    it "parses long list of quoted strings without escapes" $
      parseString (rList1 quotedString') "\"first\"\"second\"\"third\"\"fourth\"\"fifth\"" `shouldBe`
        Right [[Literal "first"], [Literal "second"], [Literal "third"], [Literal "fourth"], [Literal "fifth"]]
    it "parses literal string which ends with escaped sequence" $
      parseString (rList1 quotedString') "\"literalwithdirtyend\\\"\"" `shouldBe`
        Right [[Literal "literalwithdirtyend", Escaped "\""]]
    it "parses literal strings separated by escaped sequence" $
      parseString quotedString' "\"smells like\\\"Teen spirit\"" `shouldBe`
        Right [Literal "smells like", Escaped "\"", Literal "Teen spirit"]
    it "parses literal strings separated by escaped sequences" $
      parseString (rList1 quotedString') "\"smells like\\\"teen\\\"spirit\"" `shouldBe`
        Right [[Literal "smells like", Escaped "\"", Literal "teen", Escaped "\"", Literal "spirit"]]
    it "prints simple string correctly" $
      unparseString quotedString' [Literal "smells like"]
        `shouldBe` Just "\"smells like\""
    it "prints escaped sequence correctly" $
      unparseString quotedString' [Escaped "\""]
        `shouldBe` Just "\"\\\"\""
    it "prints escaped sequences correctly" $
      unparseString quotedString' [Escaped "\"\""]
        `shouldBe` Just "\"\\\"\\\"\""
    it "prints literal which ends with escape sequence" $
      unparseString quotedString' [Literal "smells like", Escaped "\""]
        `shouldBe` Just "\"smells like\\\"\""
    it "prints literal which ends with escape sequences" $
      unparseString quotedString' [Literal "smells like", Escaped "\"\""]
        `shouldBe` Just "\"smells like\\\"\\\"\""
    it "prints literal strings separated by escape sequence" $
      unparseString quotedString' [Literal "smells like", Escaped "\"", Literal "Teen spirit"]
        `shouldBe` Just "\"smells like\\\"Teen spirit\""
    it "prints literal strings separated by escape sequences" $
      unparseString quotedString' [Literal "smells like", Escaped "\"\"", Literal "Teen spirit"]
        `shouldBe` Just "\"smells like\\\"\\\"Teen spirit\""
    it "parses literal strings separated by escape sequences" $
      parseString quotedString' "\"smells like\\\"\\\"Teen spirit\"" `shouldBe`
        Right [Literal "smells like", Escaped "\"\"", Literal "Teen spirit"]
    it "prints literal strings separated by escape sequences and escape sequence at the end" $
      unparseString quotedString' [Literal "smells like", Escaped "\"\"", Literal "Teen spirit", Escaped "\""]
        `shouldBe` Just "\"smells like\\\"\\\"Teen spirit\\\"\""
