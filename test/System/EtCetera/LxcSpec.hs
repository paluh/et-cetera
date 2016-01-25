{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.LxcSpec where

import qualified Data.HashMap.Strict as HashMap
import           System.EtCetera.Lxc (configLines, ConfigLine(..),
                                      parseConfig, Switch(..), Value(..))
import           Text.Boomerang.String (parseString, unparseString)
import           Test.Hspec (describe, it, shouldBe, Spec)

suite :: Spec
suite = do
  describe "System.EtCetera.Lxc boomerang" $ do
    it "parses single option line with new line char at the end" $
      parseString configLines "lxc.rootfs = /mnt/rootfs.complex\n" `shouldBe`
        Right [OptionLine "lxc.rootfs" (VText "/mnt/rootfs.complex")]
    it "parses single option line without new line char at the end" $
      parseString configLines "lxc.rootfs = /mnt/rootfs.complex" `shouldBe`
        Right [OptionLine "lxc.rootfs" (VText "/mnt/rootfs.complex")]
    it "parses comment line correctly" $
      parseString configLines "# some comment\n" `shouldBe`
        Right [CommentLine " some comment"]
    it "parses empty line correctly" $
      parseString configLines "  \t\n" `shouldBe`
        Right [EmptyLine]
    it "parses two empty lines correctly" $
      parseString configLines "  \t\n\t\n" `shouldBe`
        Right [EmptyLine, EmptyLine]
    it "parses two non empty lines correctly" $
      parseString configLines (unlines [ "lxc.rootfs = /mnt/rootfs.complex"
                                       , "# another comment "
                                       ]) `shouldBe`
        Right [ OptionLine "lxc.rootfs" (VText "/mnt/rootfs.complex")
              , CommentLine " another comment "
              ]
    it "parses multiple mixed lines correctly" $
      parseString configLines (unlines [ "#comment "
                                       , ""
                                       , "lxc.rootfs = /mnt/rootfs.complex"
                                       , "# another comment "
                                       , "\t"
                                       ]) `shouldBe`
        Right [ CommentLine "comment "
              , EmptyLine
              , OptionLine "lxc.rootfs" (VText "/mnt/rootfs.complex")
              , CommentLine " another comment "
              , EmptyLine]
    it "parses switch value correctly" $
      parseString configLines (unlines [ "lxc.autodev = 1"
                                       , "# another comment "
                                       ]) `shouldBe`
        Right [ OptionLine "lxc.autodev" (VSwitch On)
              , CommentLine " another comment "
              ]
    it "prints option line" $
      unparseString configLines
                    [ CommentLine "comment "
                    , EmptyLine
                    , OptionLine "lxc.rootfs" (VText "/mnt/rootfs.complex")
                    , CommentLine " another comment "
                    , EmptyLine
                    ] `shouldBe`
      (Just . unlines $ [ "#comment "
                        , ""
                        , "lxc.rootfs=/mnt/rootfs.complex"
                        , "# another comment "
                        , ""
                        ])
  describe "System.EtCetera.Lxc parsing function" $ -- do
    it "parses multiple mixed lines correctly" $
      parseConfig (unlines [ "#comment "
                           , "lxc.include = /var/lib/lxc/lxc-common.conf"
                           , "lxc.include = /var/lib/lxc/custom"
                           , "lxc.rootfs = /mnt/rootfs.complex"
                           , "# another comment "
                           , "\t"
                           ]) `shouldBe`
        (Right . HashMap.fromList $
          [ ("lxc.include", VListOfTextValues [ "/var/lib/lxc/custom"
                                              , "/var/lib/lxc/lxc-common.conf"])
          , ("lxc.rootfs", VText "/mnt/rootfs.complex")
          ])
