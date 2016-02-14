-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
-- we are deriving Monoid instance for Config type
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}
module System.EtCetera.Redis.V2_8 where

import           Control.Applicative (Alternative)
import           Control.Arrow (first)
import           Control.Category (Category, id, (.))
import           Control.Error (note)
import           Control.Lens (DefName(..), Lens', lensField, lensRules, makeLensesWith, over, set,
                               view, (&), (.~), (??))
import           Data.Char (toLower, toUpper)
import           Data.List (foldl')
import           Data.Monoid ((<>))
import           Generics.Deriving.Base (Generic)
import           Generics.Deriving.Monoid (gmappend, gmempty, GMonoid)
import           Generics.Deriving.Show (gshow, GShow)
import           Language.Haskell.TH (mkName, Name, nameBase)
import           Prelude hiding ((.), id)
import           Text.Boomerang.Combinators (manyl, manyr, opt, push, rCons, rList, rList1, rListSep, rNil, somel, somer)
import           Text.Boomerang.HStack (arg, (:-)(..))
import           Text.Boomerang.Prim (Boomerang(..), Parser(..), prs, xpure)
import           Text.Boomerang.String (anyChar, digit, lit, int, StringBoomerang, StringError)
import           System.EtCetera.Internal (Optional(..), repeatableScalar, scalar, vector)
import           System.EtCetera.Internal.Prim (Ser(..), toPrs)
import           System.EtCetera.Internal.Boomerangs (eol, ignoreWhen, noneOf, parseString, whiteSpace,
                                                      word, (<?>))
import           System.EtCetera.Internal.Utils (capitalize)

type IP = String
type Port = Int

data SizeUnit =
    K  | M | G
  | Kb | Mb | Gb
  deriving (Eq, Read, Show)

data Size = Size Int SizeUnit
  deriving (Eq, Show)

sizeBmg :: StringBoomerang r (Size :- r)
sizeBmg =
  xpure (arg (arg (:-)) Size)
        (\(Size i u :- r) -> Just (i :- u :- r))
  . int
  . simpleSumBmg ["k", "m", "g", "kb", "mb", "gb"]

data RenameCommand =
    RenameCommand String String
  | DisableCommand String
  deriving (Eq, Show)

renameCommandBmg :: StringBoomerang r (RenameCommand :- r)
renameCommandBmg =
   c . string . somel whiteSpace . string
 where
  c =
    xpure
      (arg (arg (:-)) (\c n ->
                        if n == "\"\""
                          then DisableCommand c
                          else RenameCommand c n))
      (\case
        (DisableCommand c :- r) -> Just (c :- "\"\"" :- r)
        (RenameCommand o n :- r)   -> Just (o :- n :- r))

data MemoryPolicy =
    VolatileLru
  | AllkeysLru
  | VolatileRandom
  | AllkeysRandom
  | VolatileTtl
  | Noeviction
  deriving (Eq, Show)

memoryPolicyBmg :: StringBoomerang r (MemoryPolicy :- r)
memoryPolicyBmg =
  xpure (arg (:-) p)
        (\(mp :- r) -> (Just (s mp :- r)))
  . (word "volatile-lru" <> word "allkeys-lru"
     <> word "volatile-random" <> word "allkeys-random"
     <> word "volatile-ttl" <> word "noeviction")
 where
  p "volatile-lru"    = VolatileLru
  p "allkeys-lru"     = AllkeysLru
  p "volatile-random" = VolatileRandom
  p "allkeys-random"  = AllkeysRandom
  p "volatile-ttl"    = VolatileTtl
  p "noeviction"      = Noeviction

  s VolatileLru    = "volatile-lru"
  s AllkeysLru     = "allkeys-lru"
  s VolatileRandom = "volatile-random"
  s AllkeysRandom  = "allkeys-random"
  s VolatileTtl    = "volatile-ttl"
  s Noeviction     = "noeviction"

data LogLevel = Debug | Verbose | Notice | Warning
  deriving (Eq, Read, Show)

simpleSumBmg :: (Show a, Read a) => [String] -> StringBoomerang r (a :- r)
simpleSumBmg labels =
    xpure (arg (:-) (read . capitalize))
        (Just . arg (:-) (map toLower . show))
    . (foldl' (<>) mempty . map word $ labels)

logLevelBmg :: StringBoomerang r (LogLevel :- r)
logLevelBmg = simpleSumBmg ["debug", "verbose", "notice", "warning"]

data SyslogFacility =
    User | Local0 | Local1
  | Local2 | Local3 | Local4 | Local5
  | Local6 | Local7
  deriving (Eq, Read, Show)

syslogFacilityBmg :: StringBoomerang r (SyslogFacility :- r)
syslogFacilityBmg =
  simpleSumBmg [ "user", "local0", "local1", "local2", "local3"
               , "local4", "local5", "local6" ,   "local7"]

data Save = Save Int Int | SaveReset
  deriving (Eq, Show)

saveBmg =
  s <> sr
 where
  s = xpure (arg (arg (:-)) Save)
        (\case
          (Save s c :- r) -> Just (s :- c :- r)
          otherwise       -> Nothing)
      . int . somel whiteSpace . int
  sr = push SaveReset . lit "\"\""

string :: StringBoomerang r (String :- r)
string = rList1 (noneOf "\n \t")

slaveOfBmg :: StringBoomerang r ((IP, Port) :- r)
slaveOfBmg =
  xpure (arg (arg (:-)) (\ip port -> (ip, port)))
        (\((ip, port) :- r) -> Just (ip :- port :- r))
  . string . somel whiteSpace . int

data AppendFsync = Always | No | Everysec
  deriving (Eq, Read, Show)

appendFsyncBmg :: StringBoomerang r (AppendFsync :- r)
appendFsyncBmg = simpleSumBmg ["always", "no", "everysec"]

data RedisConfig =
  RedisConfig
    {
      include :: [FilePath]
    , daemonize :: Optional Bool
    , port :: Optional Int
    , pidFile :: Optional FilePath
    , tcpBacklog :: Optional Int
    , bind :: [String]
    , unixSocket :: Optional FilePath
    -- XXX: add more type safety
    , unixSocketPerm :: Optional String
    , timeout :: Optional Int
    , tcpKeepAlive :: Optional Int
    , logLevel :: Optional LogLevel
    , logFile :: Optional FilePath
    , syslogEnabled :: Optional Bool
    , syslogIdent :: Optional String
    , syslogFacility :: Optional SyslogFacility
    , databases :: Optional Int
    , save :: [Save]
    , stopWritesOnBgsaveError :: Optional Bool
    , rdbCompression :: Optional Bool
    , rdbChecksum :: Optional Bool
    , dbFilename :: Optional FilePath
    , dir :: Optional FilePath
    , slaveOf :: Optional (IP, Port)
    , masterAuth :: Optional String
    , slaveServeStaleData :: Optional Bool
    , slaveReadOnly :: Optional Bool
    , replDisklessSync :: Optional Bool
    , replDisklessSyncDelay :: Optional Int
    , replPingSlavePeriod :: Optional Int
    , replTimeout :: Optional Int
    , replDisableTcpNoDelay :: Optional Bool
    , replBacklogSize :: Optional Size
    , replBacklogTtl :: Optional Int
    , slavePriority :: Optional Int
    , minSlavesToWrite :: Optional Int
    , minSlavesMaxLag :: Optional Int
    , requirePass :: Optional String
    , renameCommand :: [RenameCommand]
    , maxClients :: Optional Int
    , maxMemory :: Optional Int
    , maxMemoryPolicy :: Optional MemoryPolicy
    , maxMemorySamples :: Optional Int
    , appendOnly :: Optional Bool
    , appendFilename :: Optional String
    , appendFsync :: Optional AppendFsync
    , noAppendFsyncOnRewrite :: Optional Bool
    , autoAofRewritePercentage :: Optional Int
    , autoAofRewriteMinSize :: Optional Size
    , aofLoadTruncated :: Optional Bool
    , luaTimeLimit :: Optional Int
    , slowlogLogSlowerThan :: Optional Int
    , slowlogMaxLen :: Optional Int
    , latencyMonitorThreshold :: Optional Int
    }
  deriving (Eq, Generic, Show)

instance GMonoid RedisConfig

instance Monoid RedisConfig where
  mempty = gmempty
  mappend = gmappend

makeLensesWith ?? ''RedisConfig $ lensRules
    & lensField .~ \_ _ name -> [TopName (mkName $ nameBase name ++ "Lens")]

emptyConfig = mempty

data ParsingError = ParserError StringError
  deriving (Eq, Show)

data SerializtionError = SerializtionError
  deriving (Eq, Show)

optionBoomerang label valueBoomerang =
  lit label . somel whiteSpace . valueBoomerang

strings :: StringBoomerang r ([String] :- r)
strings =
  rListSep string (somer whiteSpace)

bool :: StringBoomerang r (Bool :- r)
bool = xpure (arg (:-) (\case
                            "no" -> False
                            "yes" -> True))
               (\case
                  (True :- r)  -> Just ("yes" :- r)
                  (False :- r) -> Just ("no" :- r))
       . (word "yes" <> word "no")


(anyOptionParser, serializer) =
  repeatableScalar includeLens (optionBoomerang "include" string) .
  scalar daemonizeLens (optionBoomerang "daemonize" bool) .
  scalar portLens (optionBoomerang "port" int) .
  scalar pidFileLens (optionBoomerang "pidfile" string) .
  scalar tcpBacklogLens (optionBoomerang "tcp-backlog" int) .
  vector bindLens (optionBoomerang "bind" strings) .
  scalar unixSocketLens (optionBoomerang "unixsocket" string) .
  scalar unixSocketPermLens (optionBoomerang "unixsocketperm" string) .
  scalar timeoutLens (optionBoomerang "timeout" int) .
  scalar tcpKeepAliveLens (optionBoomerang "tcp-keepalive" int) .
  scalar logLevelLens (optionBoomerang "loglevel" logLevelBmg) .
  scalar logFileLens (optionBoomerang "logfile" string) .
  scalar syslogEnabledLens (optionBoomerang "syslog-enabled" bool) .
  scalar syslogIdentLens (optionBoomerang "syslog-ident" string) .
  scalar syslogFacilityLens (optionBoomerang "syslog-facility" syslogFacilityBmg) .
  scalar databasesLens (optionBoomerang "databases" int) .
  repeatableScalar saveLens (optionBoomerang "save" saveBmg) .
  scalar stopWritesOnBgsaveErrorLens
    (optionBoomerang "stop-writes-on-bg-save-error" bool) .
  scalar rdbCompressionLens (optionBoomerang "rdbcompression" bool) .
  scalar rdbChecksumLens (optionBoomerang "rdbchecksum" bool) .
  scalar dbFilenameLens (optionBoomerang "dbfilename" string) .
  scalar dirLens (optionBoomerang "dir" string) .
  scalar slaveOfLens (optionBoomerang "slaveof" slaveOfBmg) .
  scalar masterAuthLens (optionBoomerang "masterauth" string) .
  scalar slaveServeStaleDataLens (optionBoomerang "slave-serve-stale-data" bool) .
  scalar slaveReadOnlyLens (optionBoomerang "slave-read-only" bool) .
  scalar replDisklessSyncLens (optionBoomerang "repl-diskless-sync" bool) .
  scalar replDisklessSyncDelayLens (optionBoomerang "repl-diskless-sync-delay" int) .
  scalar replPingSlavePeriodLens (optionBoomerang "repl-ping-slave-period" int) .
  scalar replTimeoutLens (optionBoomerang "repl-timeout" int) .
  scalar replDisableTcpNoDelayLens (optionBoomerang "repl-disable-tcp-no-delay" bool) .
  scalar replBacklogSizeLens (optionBoomerang "repl-backlog-size" sizeBmg) .
  scalar replBacklogTtlLens (optionBoomerang "repl-backlog-ttl" int) .
  scalar slavePriorityLens (optionBoomerang "slave-priority" int) .
  scalar minSlavesToWriteLens (optionBoomerang "min-slaves-to-write" int) .
  scalar minSlavesMaxLagLens (optionBoomerang "min-slaves-max-lag" int) .
  scalar requirePassLens (optionBoomerang "requirepass" string) .
  repeatableScalar renameCommandLens (optionBoomerang "rename-command" renameCommandBmg) .
  scalar maxClientsLens (optionBoomerang "maxclients" int) .
  scalar maxMemoryLens (optionBoomerang "maxmemory" int) .
  scalar maxMemoryPolicyLens (optionBoomerang "maxmemory-policy" memoryPolicyBmg) .
  scalar maxMemorySamplesLens (optionBoomerang "maxmemory-samples" int) .
  scalar appendOnlyLens (optionBoomerang "appendonly" bool) .
  scalar appendFilenameLens (optionBoomerang "appendfilename" string) .
  scalar appendFsyncLens (optionBoomerang "appendfsync" appendFsyncBmg) .
  scalar noAppendFsyncOnRewriteLens (optionBoomerang "no-appendfsync-on-rewrite" bool) .
  scalar autoAofRewritePercentageLens (optionBoomerang "auto-aof-rewrite-percentage" int) .
  scalar autoAofRewriteMinSizeLens (optionBoomerang "auto-aof-rewrite-min-size" sizeBmg) .
  scalar aofLoadTruncatedLens (optionBoomerang "aof-load-truncated" bool) .
  scalar luaTimeLimitLens (optionBoomerang "lua-time-limit" int) .
  scalar slowlogLogSlowerThanLens (optionBoomerang "slowlog-log-slower-than" int) .
  scalar slowlogMaxLenLens (optionBoomerang "slowlog-max-len" int) .
  scalar latencyMonitorThresholdLens (optionBoomerang "latency-monitor-threshold" int)
  $ (mempty, id)

parser =
  -- comment
  ((toPrs (lit "#" . manyr (ignoreWhen (/= '\n')))
  -- empty line - I'm losing error info by matching just "manyl whiteSpace"
   <> toPrs (somel whiteSpace)
  -- option
   <> anyOptionParser <?>
      (\t -> "can't parse any option from this line: " ++ takeWhile (/= '\n') t))
  -- end of line and more options
  -- or optinal end of line
   . (id <> eol' <> (eol' . parser)))
  -- completely empty line (I don't want to handle this
  -- above with `manyl witeSpace` because always matches
  -- and causes errors information loss
   <> (eol' . parser)
  where
    -- used only for parser generation
    eol' = toPrs eol

parse :: String -> Either ParsingError RedisConfig
parse conf =
  either (Left . ParserError) Right (parseString (parser . toPrs (push emptyConfig)) conf)

serialize :: RedisConfig -> Either SerializtionError String
serialize redisConfig =
  note SerializtionError . fmap (($ "") . fst) . s $ (redisConfig :- ())
 where
  Ser s = serializer
