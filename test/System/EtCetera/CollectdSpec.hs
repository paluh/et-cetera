{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.CollectdSpec where

import           Control.Category ((.)) --, id)
import           Data.Maybe (fromMaybe)
import           Prelude hiding ((.), id)
import           System.EtCetera.Collectd (argumentList, comment, cpu, CPU(..),
                                           disk, Disk(..), foldStringOption, globals,
                                           Globals(..), option, number,
                                           option, ConfigOption(..),
                                           Option(..),
                                           options, quotedString,
                                           QuotedStringPart(..),
                                           Value(..))
import           Text.Boomerang.Combinators (push, rList1)
import           Text.Boomerang.HStack ((:-)(..))
import           Text.Boomerang.Prim (unparse)
import           Text.Boomerang.String (unparseString, parseString)
import           Test.Hspec (describe, it, shouldBe, Spec)

main :: IO ()
main = do
  print "test"
  --print $ unparseString (option "") (Option "LoadPlugin" [StringValue "df"] [] [])
  --print $ unparseString (options "") [ Option "LoadPlugin" [StringValue "df"] [] []
  --                              , Option "LoadPlugin" [StringValue "df"] [] []
  --                              , Option "LoadPlugin" [StringValue "load"] [] []
  --                              , Option "LoadPlugin" [StringValue "ping"] [] []
  --                              ]
  -- print $ unparseString (option "") (Option "LoadPlugin" [StringValue "df"]
  --                                                    [Option "Test" [StringValue "8", IntValue 8] []])

  let x = fromMaybe "" $ unparseString (options "") [Option "Option1" [StringValue "df"]
                                                       [Option "Suboption" [StringValue "1"] [],
                                                        Option "Subsection2" [StringValue "2"] [],
                                                        Option "Suboption" [StringValue "3"] []],
                                                     Option "Option3" []
                                                       [Option "Subsection4" [StringValue "lklj"] [
                                                           Option "Subsuboption" [StringValue "9"] [],
                                                           Option "Subsuboption" [] []]]]
  putStr x
  -- print $ unparseString (options "") [Option "Option1" [StringValue "df"]
  --                                                     [Option "Option1" [StringValue "1"] [],
  --                                                      Option "Option2" [StringValue "2"] [],
  --                                                      Option "Option3" [StringValue "3"] []],
  --                                Option "Option2" [] [Option "Ble" [] [Option "T" [StringValue "9"] [], Option "Z" [] []]]]
  -- print $ parseString options' "\nTest 8"
  -- print $ parseString (option "") "<Plugin arg1 1 2>\nChild1 2.8\n</Plugin>"

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
      parseString (option "") "LoadPlugin cpu" `shouldBe`
        Right (ConfigOption "LoadPlugin" [StringValue "cpu"])
    it "parses simple option with single quoted argument" $
      parseString (option "") "LoadPlugin \"cpu\"" `shouldBe`
        Right (ConfigOption "LoadPlugin" [StringValue "cpu"])
    it "parses option with multiple unquoted arguments" $
      parseString (option "") "DriverOption host localhost" `shouldBe`
        Right (ConfigOption "DriverOption" [StringValue "host", StringValue "localhost"])
    it "parses option with multiple quoted simple arguments" $
      parseString (option "") "DriverOption \"host\" \"localhost\"" `shouldBe`
        Right (ConfigOption "DriverOption" [StringValue "host", StringValue "localhost"])
    it "parses option with multiple quoted paths" $
      parseString (option "") "Include \"/etc/collectd.d/*\" \"localhost\"" `shouldBe`
        Right (ConfigOption "Include" [StringValue "/etc/collectd.d/*", StringValue "localhost"])
    it "parses empty section" $
      parseString (option "") "<Plugin>\n</Plugin>" `shouldBe`
        Right (ConfigOption "Plugin" [])
    it "parses section with arguments" $
      parseString (option "") "<Plugin arg1 1 2>\n</Plugin>" `shouldBe`
        Right (ConfigOption "Plugin" [StringValue "arg1", IntValue 1, IntValue 2])
    it "parses section with arguments and children" $
      parseString (option "") "<Plugin arg1 1 2>\nChild1 2.8\n</Plugin>" `shouldBe`
        Right (ConfigSection "Plugin" [StringValue "arg1", IntValue 1, IntValue 2] [ConfigOption "Child1" [FloatValue 2.8]])
    it "parses section with muliline children" $
       parseString (option "") "<Plugin arg1 1 2>\nChild1 2.8\\\n 8 9\n</Plugin>" `shouldBe`
         Right (ConfigSection "Plugin" [StringValue "arg1", IntValue 1, IntValue 2]
                                       [ConfigOption "Child1" [FloatValue 2.8, IntValue 8, IntValue 9]])
  describe "EtCetera.Collectd.comment boomerang" $ -- do
    it "parses single comment" $
      parseString (comment . push ()) "    #just comment\n" `shouldBe`
        Right ()
  describe "EtCetera.Collectd.options boomerang" $ do
    it "parses single comment" $
      parseString (options "") "    #just comment\n" `shouldBe`
        Right []
    it "parses option ended with comment" $
      parseString (options "") "LoadPlugin cpu #some comment\n" `shouldBe`
        Right [Option "LoadPlugin" [StringValue "cpu"] []]
    it "parses options separated by comments" $
      parseString (options "") ("LoadPlugin cpu\n" ++
                           "# first comment\n" ++
                           "LoadPlugin load\n" ++
                           "# second comment\n" ++
                           "LoadPlugin ping\n") `shouldBe`
        Right [ Option "LoadPlugin" [StringValue "cpu"] []
              , Option "LoadPlugin" [StringValue "load"] []
              , Option "LoadPlugin" [StringValue "ping"] []
              ]
    it "parses options and sections separated by comments" $
      parseString (options "") ("LoadPlugin cpu\n" ++
                           "# first comment\n" ++
                           "LoadPlugin load\n" ++
                           "# second comment\n" ++
                           "<Plugin ping>\n" ++
                           " Host \"example.org\"\n" ++
                           "</Plugin>\n" ++
                           "# third comment\n" ++
                           "LoadPlugin ping\n") `shouldBe`
        Right
          [ Option "LoadPlugin" [StringValue "cpu"] []
          , Option "LoadPlugin" [StringValue "load"] []
          , Option "Plugin" [StringValue "ping"] [Option "Host" [StringValue "example.org"] []]
          , Option "LoadPlugin" [StringValue "ping"] []
          ]

    it "prints single option" $
      unparseString (options "") [ Option "LoadPlugin" [StringValue "cpu"] []
                            ] `shouldBe` Just "LoadPlugin cpu"
    it "prints multiple options" $
      unparseString (options "") [ Option "LoadPlugin" [StringValue "cpu"] []
                            , Option "LoadPlugin" [StringValue "load"] []
                            , Option "LoadPlugin" [StringValue "ping"] []
                            ] `shouldBe` Just "LoadPlugin cpu\nLoadPlugin load\nLoadPlugin ping"
    it "prints single section" $
      unparseString (options "")
        [ Option "LoadPlugin" [StringValue "df"]
            [ Option "Interval" [StringValue "arg1"] []
            , Option "Option" [] [Option "Subsection" [] [Option "Option" [] [], Option "Option2" [] [], Option "Subsubsection" [] [Option "bleble" [IntValue 999] []]]]
            ]
        ] `shouldBe` Just "<LoadPlugin df>\n\tInterval arg1\n\t<Option>\n\t\t<Subsection>\n\t\t\tOption\n\t\t\tOption2\n\t\t\t<Subsubsection>\n\t\t\t\tbleble 999\n\t\t\t</Subsubsection>\n\t\t</Subsection>\n\t</Option>\n</LoadPlugin>"

  describe "EtCetera.Collectd.globals boomerang" $ do
    it "parses globals with all options" $
      parseString (globals . options "") "baseDir \"/home/paluh/collectd/\"\nautoLoadPlugin true" `shouldBe`
        Right (Just (Globals (Just True) "/home/paluh/collectd/"))
    it "prints globals with all options" $
      unparseString (globals . options "") (Just (Globals (Just True) "/home/paluh/collectd/")) `shouldBe`
        Just "autoLoadPlugin true\nbaseDir \"/home/paluh/collectd/\""
    it "parses globals with missing option" $
      parseString (globals . options "") "baseDir \"/home/paluh/collectd/\"" `shouldBe`
        Right (Just (Globals Nothing "/home/paluh/collectd/"))
    it "prints globals with missing option" $
      unparseString (globals . options "") (Just (Globals Nothing "/home/paluh/collectd/")) `shouldBe`
        Just "baseDir \"/home/paluh/collectd/\""


  describe "EtCetera.Collectd.cpu boomerang" $ do
    it "prints cpu plugin" $
      unparseString (cpu . options "") (Just (CPU (Just True) (Just True) Nothing)) `shouldBe`
        Just ("<Plugin cpu>\n" ++
                "\tReportByState true\n" ++
                "\tReportByCPU true\n" ++
              "</Plugin>")
    it "parses cpu plugin" $
      parseString (cpu . options "")
        ("<Plugin cpu>\n" ++
            "ReportByState true\n" ++
            "\n" ++
            " #some comment  \n" ++
            " ReportByCPU true  \n" ++
            "\tValuePercentage false\n" ++
          "</Plugin>  \n  \n") `shouldBe`
        Right (Just (CPU (Just True) (Just True) (Just False)))

  describe "EtCetera.Collectd.foldStringOption" $ do
    it "aggregates values correctly" $
      parseString (foldStringOption "Disk" . options "")
        ("Disk sdd\n" ++
         "Disk \"/hda[34]/\"\n" ++
         "Disk sda8\n") `shouldBe`
        Right (Just [Option "Disk" [StringListValue ["sdd", "/hda[34]/", "sda8"]] []])
    it "aggregates values correctly" $
      parseString (disk . options "")
        ("<Plugin disk>\n" ++
         "Disk sdd\n" ++
         "Disk \"/hda[34]/\"\n" ++
         "Disk sda8\n" ++
         "IgnoreSelected true\n" ++
         "UseBSDName false\n" ++
         "UdevNameAttr \"DM_NAME\"" ++
         "</Plugin>") `shouldBe`
        Right (Just (Disk ["sdd", "/hda[34]/", "sda8"] (Just True) (Just False) (Just "DM_NAME")))
