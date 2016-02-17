{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.LxcSpec where

import           Control.Monad.Trans (liftIO)
import           Data.List (intercalate)
import           System.EtCetera.Internal (Optional(..))
import           System.EtCetera.Lxc.Internal (Arch(..), emptyConfig, emptyNetwork, LxcConfig,
                                               Network(..), NetworkType(..), parse, serialize,
                                               SerializtionError(..), Switch(..),
                                               LxcConfig(..))
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
        (Right $ emptyConfig { lxcNetwork = [emptyNetwork { lxcNetworkType = Present Macvlan } ] })
    it "parses multiple network declarations" $
      parse (unlines [ "lxc.network.type = macvlan"
                     , "lxc.network.name = eth0"
                     , "lxc.network.type = veth"
                     , "lxc.network.name = eth1"
                     ]) `shouldBe`
        (Right $ emptyConfig { lxcNetwork = [ emptyNetwork { lxcNetworkType = Present Macvlan
                                                           , lxcNetworkName = Present "eth0"}
                                            , emptyNetwork { lxcNetworkType = Present Veth
                                                           , lxcNetworkName = Present "eth1"}] } )
    it "parses integer value correctly" $
      parse "lxc.start.delay = 8" `shouldBe`
        (Right $ emptyConfig { lxcStartDelay = Present 8})
    it "parses multiple options" $
      parse (unlines [ "lxc.aa_profile=/mnt/rootfs.complex"
                     , "lxc.include=/var/lib/lxc/lxc-common.conf"
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
    it "parsing regression 2016.02.18.1" $
      parse (intercalate "\n" [ "lxc.arch=x86_64\n"
                              , "lxc.rootfs=/var/lib/lxc/stream6-clone.nadaje.com/rootfs"
                              , "lxc.utsname=stream6-clone.nadaje.com"
                              , "lxc.include=/usr/share/lxc/config/debian.common.conf"
                              , "lxc.network.type=veth"
                              , "lxc.network.flags=up"
                              , "lxc.network.ipv4=10.0.0.3"
                              , "lxc.network.ipv4.gateway=10.0.0.2"
                              , "lxc.network.link=lxc-br01"
                              , "lxc.network.name=eth0"
                              , "\n"
                              , "\n"
                              ]) `shouldBe`
        (Right
           emptyConfig
             { lxcArch = Present X86_64
             , lxcRootfs = Present "/var/lib/lxc/stream6-clone.nadaje.com/rootfs"
             , lxcUtsname = Present "stream6-clone.nadaje.com"
             , lxcInclude = ["/usr/share/lxc/config/debian.common.conf"]
             , lxcNetwork = [ emptyNetwork
                              { lxcNetworkType = Present Veth
                              , lxcNetworkFlags = Present "up"
                              , lxcNetworkIpv4 = Present "10.0.0.3"
                              , lxcNetworkIpv4Gateway = Present "10.0.0.2"
                              , lxcNetworkLink = Present "lxc-br01"
                              , lxcNetworkName = Present "eth0"
                              }
                            ]
             })
  describe "System.EtCetera.Lxc serialize" $ do
    it "serializes multiple options" $
      serialize (emptyConfig { lxcInclude = [ "/var/lib/lxc/lxc-common.conf"
                                            , "/var/lib/lxc/custom"]
                             , lxcAaProfile = Present "/mnt/rootfs.complex"
                             }) `shouldBe`
        (Right . unlines $ [ "lxc.aa_profile=/mnt/rootfs.complex"
                           , "lxc.include=/var/lib/lxc/lxc-common.conf"
                           , "lxc.include=/var/lib/lxc/custom"
                           ])

    it "serializes multiple correct networks" $
      serialize
        (emptyConfig
           { lxcNetwork = [ emptyNetwork { lxcNetworkType = Present Macvlan }
                          , emptyNetwork { lxcNetworkType = Present Veth
                                         , lxcNetworkName = Present "eth0" }]
           }) `shouldBe`
        Right "lxc.network.type=macvlan\nlxc.network.type=veth\nlxc.network.name=eth0\n"
