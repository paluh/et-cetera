{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.LxcSpec where

import           System.EtCetera.Lxc.Internal (configLines, ConfigLine(..), emptyConfig, LxcConfig,
                                               NetworkType(..), parse, serialize,
                                               SerializtionError(..), Switch(..), Value(..),
                                               lxcInclude, lxcNetworkType, lxcRootfs, lxcStartDelay)
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
  describe "System.EtCetera.Lxc parsing function" $ do
    it "parses multiple mixed lines correctly" $
      parse (unlines [ "#comment "
                     , "lxc.include = /var/lib/lxc/lxc-common.conf"
                     , "lxc.include = /var/lib/lxc/custom"
                     , "lxc.rootfs = /mnt/rootfs.complex"
                     , "# another comment "
                     , "\t"
                     ]) `shouldBe`
        (Right $ emptyConfig { lxcInclude = [ "/var/lib/lxc/custom"
                                            , "/var/lib/lxc/lxc-common.conf"]
                             , lxcRootfs =  Just "/mnt/rootfs.complex"
                             })
    it "parses network type correctly" $
      parse "lxc.network.type = macvlan" `shouldBe`
        (Right $ emptyConfig { lxcNetworkType = Just Macvlan})
    it "parses integer value correctly" $
      parse "lxc.start.delay = 8" `shouldBe`
        (Right $ emptyConfig { lxcStartDelay = Just 8})
  describe "System.EtCetera.Lxc serialization function" $ -- do
    it "serializes multiple mixed options correctly" $
      serialize (emptyConfig { lxcInclude = [ "/var/lib/lxc/custom"
                                            , "/var/lib/lxc/lxc-common.conf"]
                             , lxcRootfs =  Just "/mnt/rootfs.complex"
                             }) `shouldBe`
        (Right . unlines $ [ "lxc.rootfs=/mnt/rootfs.complex"
                           , "lxc.include=/var/lib/lxc/lxc-common.conf"
                           , "lxc.include=/var/lib/lxc/custom"
                           ])
