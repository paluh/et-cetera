module System.EtCetera.RedisSpec where

import           Data.Either (isLeft)
import           System.EtCetera.Internal (Optional(..))
import           System.EtCetera.Redis.V2_8 (bind, daemonize, emptyConfig, parse,
                                             include, serialize)
import           Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import           Text.Boomerang.Error (ParserError(..))


suite :: Spec
suite = do
  describe "System.EtCetera.Redis parse" $ do
    it "parses single option line without new line" $
      parse "include /var/lib/redis/custom.conf" `shouldBe`
        Right (emptyConfig {include = ["/var/lib/redis/custom.conf"]})
    it "parses single vector option line without new line" $
      parse "bind 127.0.0.1 192.168.1.1" `shouldBe`
        Right (emptyConfig {bind = ["127.0.0.1", "192.168.1.1"]})
    it "parses emtpy line without new line" $
      parse "   " `shouldBe`
        Right emptyConfig
    it "parses emtpy line with new line" $
      parse "   \n" `shouldBe`
        Right emptyConfig
    it "parses multiple emtpy lines without new line" $
      parse " \n  " `shouldBe`
        Right emptyConfig
    it "parses multiple emtpy lines with new line" $
      parse " \n  \t \n\n\n \t\n\t" `shouldBe`
        Right emptyConfig
    it "failes on unparsable line" $
      parse "daemonize=test" `shouldSatisfy` isLeft
  describe "System.EtCetera.Redis serialize" $ do
    it "serializes full config" $
      serialize (emptyConfig { include = [ "/usr/share/redis/redis-common.conf"
                                              , "/var/lib/redis/custom.conf"]
                             , daemonize = Present True
                             , bind = []
                             }) `shouldBe`
        (Right . unlines $ [ "daemonize yes"
                           , "include /usr/share/redis/redis-common.conf"
                           , "include /var/lib/redis/custom.conf"
                           ])
    it "serializes non empty bind" $
      serialize (emptyConfig { include = [ "/usr/share/redis/redis-common.conf"
                                         , "/var/lib/redis/custom.conf"]
                             , daemonize = Present True
                             , bind = ["127.0.0.1", "192.168.1.1"]
                             }) `shouldBe`
        (Right . unlines $ [ "bind 127.0.0.1 192.168.1.1"
                           , "daemonize yes"
                           , "include /usr/share/redis/redis-common.conf"
                           , "include /var/lib/redis/custom.conf"
                           ])
