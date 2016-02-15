{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.LxcSpec where

import           Control.Monad.Trans (liftIO)
import           Data.List (intercalate)
import           System.EtCetera.Internal (Optional(..))
import           System.EtCetera.Lxc.Internal (emptyConfig, LxcConfig,
                                               NetworkType(..), parse, serialize,
                                               SerializtionError(..), Switch(..),
                                               lxcAaProfile, lxcInclude,
                                               lxcNetworkType, lxcRootfs,
                                               lxcStartDelay)
import           Test.Hspec (describe, it, shouldBe, Spec)

suite :: Spec
suite = do
  describe "System.EtCetera.Lxc parse" $ do
    it "parses single option line with new line char at the end" $
      parse "lxc.aa_profile=/mnt/rootfs.complex" `shouldBe`
        Right (emptyConfig {lxcAaProfile = Present "/mnt/rootfs.complex"})
    it "parses single comment without new line" $
      parse "# comment" `shouldBe`
        Right emptyConfig
    it "parses single comment with new line" $
      parse "# comment\n" `shouldBe`
        Right emptyConfig
    it "parses network type correctly" $
      parse "lxc.network.type = macvlan" `shouldBe`
        (Right $ emptyConfig { lxcNetworkType = Present Macvlan})
    it "parses integer value correctly" $
      parse "lxc.start.delay = 8" `shouldBe`
        (Right $ emptyConfig { lxcStartDelay = Present 8})
    it "parses multiple options" $
      parse (unlines [ "lxc.include=/var/lib/lxc/lxc-common.conf"
                     , "lxc.aa_profile=/mnt/rootfs.complex"
                     , "lxc.include=/var/lib/lxc/custom"
                     ]) `shouldBe`
        Right (emptyConfig { lxcAaProfile = Present "/mnt/rootfs.complex"
                           , lxcInclude = [ "/var/lib/lxc/lxc-common.conf"
                                          , "/var/lib/lxc/custom"
                                          ]})
    it "parses mixed multiple options line with new line char at the end" $
      parse (unlines [ "lxc.include=/var/lib/lxc/lxc-common.conf"
                        , "# comment"
                        , "lxc.aa_profile=/mnt/rootfs.complex"
                        , "# another comment"
                        , ""
                        , "lxc.include=/var/lib/lxc/custom"
                        ]) `shouldBe`
        Right (emptyConfig { lxcAaProfile = Present "/mnt/rootfs.complex"
                           , lxcInclude = [ "/var/lib/lxc/lxc-common.conf"
                                          , "/var/lib/lxc/custom"
                                          ]})
    it "parses mixed multiple lines without new line at the end" $
      parse (intercalate "\n" [ "#comment "
                              , "lxc.include = /var/lib/lxc/lxc-common.conf"
                              , "lxc.include = /var/lib/lxc/custom"
                              , "lxc.rootfs = /mnt/rootfs.complex"
                              , "# another comment "
                              , "\t"
                              ]) `shouldBe`
        (Right $ emptyConfig { lxcInclude = [ "/var/lib/lxc/lxc-common.conf"
                                            , "/var/lib/lxc/custom"]
                             , lxcRootfs = Present "/mnt/rootfs.complex"
                             })
  describe "System.EtCetera.Lxc serialize" $ -- do
    it "serializes multiple options" $
      serialize (emptyConfig { lxcInclude = [ "/var/lib/lxc/lxc-common.conf"
                                            , "/var/lib/lxc/custom"]
                             , lxcAaProfile = Present "/mnt/rootfs.complex"
                             }) `shouldBe`
        (Right . unlines $ [ "lxc.include=/var/lib/lxc/lxc-common.conf"
                           , "lxc.include=/var/lib/lxc/custom"
                           , "lxc.aa_profile=/mnt/rootfs.complex"
                           ])
