{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module System.EtCetera.LxcSpec where

import           System.EtCetera.Lxc (configLines, ConfigLine(..), Value(..))
import           Text.Boomerang.String (parseString, unparseString)
import           Test.Hspec (describe, it, shouldBe, Spec)

suite :: Spec
suite = -- do
  describe "System.EtCetera.Lxc boomerang" $ do
    it "parses single option line with new line char at the end" $
      parseString configLines "lxc.rootfs = /mnt/rootfs.complex\n" `shouldBe`
        Right [OptionLine "lxc.rootfs" (ValueText "/mnt/rootfs.complex")]
    it "parses single option line without new line char at the end" $
      parseString configLines "lxc.rootfs = /mnt/rootfs.complex" `shouldBe`
        Right [OptionLine "lxc.rootfs" (ValueText "/mnt/rootfs.complex")]
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
        Right [ OptionLine "lxc.rootfs" (ValueText "/mnt/rootfs.complex")
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
              , OptionLine "lxc.rootfs" (ValueText "/mnt/rootfs.complex")
              , CommentLine " another comment "
              , EmptyLine]
    it "prints option line" $
      unparseString configLines
                    [ CommentLine "comment "
                    , EmptyLine
                    , OptionLine "lxc.rootfs" (ValueText "/mnt/rootfs.complex")
                    , CommentLine " another comment "
                    , EmptyLine
                    ] `shouldBe`
      (Just . unlines $ [ "#comment "
                        , ""
                        , "lxc.rootfs=/mnt/rootfs.complex"
                        , "# another comment "
                        , ""
                        ])

