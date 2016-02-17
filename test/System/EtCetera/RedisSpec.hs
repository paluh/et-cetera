module System.EtCetera.RedisSpec where

import           Data.Either (isLeft)
import           Data.Set (fromList)
import           System.EtCetera.Internal (Optional(..))
import           System.EtCetera.Redis.V2_8 (bind, clientOutputBufferLimitNormal,
                                             clientOutputBufferLimitSlave,
                                             clientOutputBufferLimitPubSub,
                                             ClientOutputBufferLimitSize(..), daemonize, emptyConfig,
                                             parse, include, KeyspaceEvn(..), KeyspaceEvnType(..),
                                             notifyKeyspaceEvents, RenameCommand(..), renameCommand,
                                             replBacklogSize, save, Save(..), serialize, Size(..),
                                             SizeUnit(..))
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
    it "parses multiple save options" $
      parse "save 10 10\nsave 20 20\nsave  10   80\nsave \"\"" `shouldBe`
        Right (emptyConfig {save = [Save 10 10, Save 20 20, Save 10 80, SaveReset]})
    it "parses backlog size" $
      parse "repl-backlog-size 10mb" `shouldBe`
        Right (emptyConfig {replBacklogSize = Present (Size 10 Mb)})
    it "parses rename commands" $
      parse "rename-command KEY K\nrename-command CONFIG \"\"" `shouldBe`
        Right (emptyConfig {renameCommand = [ RenameCommand "KEY" "K"
                                            , DisableCommand "CONFIG"]})
    it "parses nonempty keyspace notification events configuration" $
      parse "notify-keyspace-events KE$g" `shouldBe`
        Right (emptyConfig {notifyKeyspaceEvents =
                              Present $
                                SetKespaceEvns $
                                  fromList [ KeyspaceEvns
                                           , KeyeventEvns
                                           , StringCmds
                                           , GenericCmds
                                           ]})
    it "parses keyspace notification events configuration with alias" $
      parse "notify-keyspace-events KA" `shouldBe`
        Right (emptyConfig {notifyKeyspaceEvents =
                              Present $
                                SetKespaceEvns $
                                  fromList [ KeyspaceEvns
                                           , GenericCmds
                                           , StringCmds
                                           , ListCmds
                                           , SetCmds
                                           , HashCmds
                                           , SortedSetCmds
                                           , ExpiredEvns
                                           , EvictedEvns
                                           ]})
    it "parses output buffer limits correctly" $
      (parse . unlines $ [ "client-output-buffer-limit normal 10m 20mb 20"
                         , "client-output-buffer-limit slave 10g 20gb 20"
                         , "client-output-buffer-limit pubsub 10k 20kb 20"
                         ]) `shouldBe`
      Right (emptyConfig { clientOutputBufferLimitNormal =
                            Present (ClientOutputBufferLimitSize (Size 10 M) (Size 20 Mb) 20)
                         , clientOutputBufferLimitSlave =
                            Present (ClientOutputBufferLimitSize (Size 10 G) (Size 20 Gb) 20)
                         , clientOutputBufferLimitPubSub =
                            Present (ClientOutputBufferLimitSize (Size 10 K) (Size 20 Kb) 20)
                         })
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
    it "serializes nonempty bind" $
      serialize (emptyConfig { include = [ "/usr/share/redis/redis-common.conf"
                                         , "/var/lib/redis/custom.conf"]
                             , daemonize = Present True
                             , bind = ["127.0.0.1", "192.168.1.1"]
                             }) `shouldBe`
        (Right . unlines $ [ "daemonize yes"
                           , "bind 127.0.0.1 192.168.1.1"
                           , "include /usr/share/redis/redis-common.conf"
                           , "include /var/lib/redis/custom.conf"
                           ])
    it "serializes nonempty events notification configuration" $
      serialize (emptyConfig { notifyKeyspaceEvents =
                                Present $
                                  SetKespaceEvns $
                                    fromList [ KeyspaceEvns
                                             , GenericCmds
                                             , StringCmds
                                             , ListCmds
                                             , SetCmds
                                             , HashCmds
                                             , SortedSetCmds
                                             , ExpiredEvns
                                             , EvictedEvns
                                             ]}) `shouldBe`
        Right "notify-keyspace-events exzhsl$gK\n"

    it "serializes disabled events notification" $
      serialize (emptyConfig { notifyKeyspaceEvents =
                                Present DisableKeyspaceEvns}) `shouldBe`
        Right "notify-keyspace-events \"\"\n"
    it "serializes clients buffer limits" $
      serialize (emptyConfig { clientOutputBufferLimitSlave =
                                Present (ClientOutputBufferLimitSize (Size 10 G) (Size 20 Gb) 20)
                             , clientOutputBufferLimitNormal =
                                Present (ClientOutputBufferLimitSize (Size 10 M) (Size 20 Mb) 20)
                             , clientOutputBufferLimitPubSub =
                                Present (ClientOutputBufferLimitSize (Size 10 K) (Size 20 Kb) 20)
                             }) `shouldBe`
        (Right . unlines $ [ "client-output-buffer-limit normal 10m 20mb 20"
                           , "client-output-buffer-limit slave 10g 20gb 20"
                           , "client-output-buffer-limit pubsub 10k 20kb 20"
                           ]) 
