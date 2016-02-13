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
import           Data.Monoid ((<>))
import           Generics.Deriving.Base (Generic)
import           Generics.Deriving.Monoid
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

-- 1k => 1000 bytes
-- 1kb => 1024 bytes
-- 1m => 1000000 bytes
-- 1mb => 1024*1024 bytes
-- 1g => 1000000000 bytes
-- 1gb => 1024*1024*1024 bytes
data SizeUnit =
    Kibibytes | Mebibytes | Gibibytes -- 2^10, 2^20, 2^30
  | Kilobytes | Megabytes | Gigabytes -- 10^3, 10^6, 10^9
  deriving (Eq, Show)

data Size = Size Int SizeUnit
  deriving (Eq, Show)

-- these types Show and Read instances are crucial for
-- correct parsing

data LogLevel = Debug | Verbose | Notice | Warning
  deriving (Eq, Read, Show)

logLevelBmg :: StringBoomerang r (LogLevel :- r)
logLevelBmg =
    xpure (arg (:-) (read . capitalize))
        (Just . arg (:-) (map toLower . show))
    . (word "debug" <> word  "verbose"
       <> word  "notice" <> word  "warning")

data SyslogFacility =
    User | Local0 | Local1
  | Local2 | Local3 | Local4 | Local5
  | Local6 | Local7
  deriving (Eq, Read, Show)

syslogFacilityBmg :: StringBoomerang r (SyslogFacility :- r)
syslogFacilityBmg =
  xpure (arg (:-) (read . capitalize))
      (Just . arg (:-) (map toLower . show))
  . (word "user" <> word  "local0"
     <> word  "local1" <> word  "local2"
     <> word  "local3" <> word  "local4"
     <> word  "local5" <> word  "local6"
     <> word  "local7")

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
    , maxSlavesMaxLag :: Optional Int
    , requirePass :: Optional String
    -- , renameCommand :: [Maybe String String]
    -- , maxClients :: Maybe Int
    -- , maxMemory :: Maybe Int
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

string :: StringBoomerang r (String :- r)
string = rList1 (noneOf "\n \t")

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
  repeatableScalar saveLens (optionBoomerang "save" saveBmg) -- .
  --  scalar stopWritesOnBgsaveErrorLens (optionBoomerang "stopWritesOnBgsaveError" Bool) .
  --  scalar rdbCompressionLens (optionBoomerang "rdbCompression" Bool) .
  --  scalar rdbChecksumLens (optionBoomerang "rdbChecksum" Bool) .
  --  scalar dbFilenameLens (optionBoomerang "dbFilename" FilePath) .
  --  scalar dirLens (optionBoomerang "dir" FilePath) .
  --  scalar slaveOfLens (optionBoomerang "slaveOf" (IP, Port)) .
  --  scalar masterAuthLens (optionBoomerang "masterAuth" String) .
  --  scalar slaveServeStaleDataLens (optionBoomerang "slaveServeStaleData" Bool) .
  --  scalar slaveReadOnlyLens (optionBoomerang "slaveReadOnly" Bool) .
  --  scalar replDisklessSyncLens (optionBoomerang "replDisklessSync" Bool) .
  --  scalar replDisklessSyncDelayLens (optionBoomerang "replDisklessSyncDelay" Int) .
  --  scalar replPingSlavePeriodLens (optionBoomerang "replPingSlavePeriod" Int) .
  --  scalar replTimeoutLens (optionBoomerang "replTimeout" Int) .
  --  scalar replDisableTcpNoDelayLens (optionBoomerang "replDisableTcpNoDelay" Bool) .
  --  scalar replBacklogSizeLens (optionBoomerang "replBacklogSize" Size) .
  --  scalar replBacklogTtlLens (optionBoomerang "replBacklogTtl" Int) .
  --  scalar slavePriorityLens (optionBoomerang "slavePriority" Int) .
  --  scalar minSlavesToWriteLens (optionBoomerang "minSlavesToWrite" Int) .
  --  scalar maxSlavesMaxLagLens (optionBoomerang "maxSlavesMaxLag" Int) .
  --  scalar requirePassLens (optionBoomerang "requirePass" String) .
  --  , renameCommand :: [Maybe String String]
  --  scalar maxClientsLens (optionBoomerang "maxClients" Int) .
  --  scalar maxMemoryLens (optionBoomerang "maxMemory" Int) .
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
