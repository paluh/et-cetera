{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.Lxc where

import           Control.Arrow (first)
import           Control.Category ((.), id)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (foldl')
import           Data.Monoid (mempty, (<>))
import           Prelude hiding ((.))
import           Text.Boomerang.Combinators (manyl, opt, push, rCons, rList, rList1, rNil)
import           Text.Boomerang.HStack (arg, (:-)(..))
import           Text.Boomerang.Prim (xpure)
import           Text.Boomerang.String (anyChar, char, lit, satisfy, StringBoomerang)

type Key = String
data Value = ValueText String
           | ValueInt Int
  deriving (Eq, Show)
data ConfigLine = EmptyLine | CommentLine String | OptionLine Key Value
  deriving (Eq, Show)

noneOf :: String -> StringBoomerang r (Char :- r)
noneOf l = satisfy (not . (`elem` l))

oneOf :: String -> StringBoomerang r (Char :- r)
oneOf l = satisfy (`elem` l)

eol :: StringBoomerang r r
eol = lit "\n" <> lit "\r\n"

word :: String -> StringBoomerang r (String :- r)
word []     = rNil
word (x:xs) = rCons . char x . word xs

value :: StringBoomerang r (String :- r)
value = rCons . noneOf "\n\r \t" . rList1 anyChar

text :: StringBoomerang (String :-r) (Value :- r)
text = xpure (arg (:-) ValueText)
             (\case
                (ValueText t :- r) -> Just (t :- r)
                otherwise          -> Nothing)

-- keys list taken from here:
-- https://github.com/lxc/lxc/blob/ffe344373e5d2b9f2be517f138bf42f9c7d0ca20/src/lxc/confile.c#L116
optionLine :: StringBoomerang r (ConfigLine :- r)
optionLine =
  foldl' (<>) mempty
    (map (\(l, vp) -> option . word l . manyl whiteSpace
                   .  lit "=" . manyl whiteSpace . vp . value)
      [ ("lxc.aa_allow_incomplete", text)
      , ("lxc.aa_profile", text)
      , ("lxc.arch", text)
      , ("lxc.autodev", text)
      , ("lxc.cap.drop", text)
      , ("lxc.cap.keep", text)
      , ("lxc.cgroup", text)
      , ("lxc.console", text)
      , ("lxc.console.logfile", text)
      , ("lxc.devttydir", text)
      , ("lxc.environment", text)
      , ("lxc.ephemeral", text)
      , ("lxc.group", text)
      , ("lxc.haltsignal", text)
      , ("lxc.hook", text)
      , ("lxc.hook.autodev", text)
      , ("lxc.hook.clone", text)
      , ("lxc.hook.destroy", text)
      , ("lxc.hook.mount", text)
      , ("lxc.hook.post-stop", text)
      , ("lxc.hook.pre-mount", text)
      , ("lxc.hook.pre-start", text)
      , ("lxc.hook.start", text)
      , ("lxc.hook.stop", text)
      , ("lxc.id_map", text)
      , ("lxc.include", text)
      , ("lxc.init_cmd", text)
      , ("lxc.init_gid", text)
      , ("lxc.init_uid", text)
      , ("lxc.kmsg", text)
      , ("lxc.logfile", text)
      , ("lxc.loglevel", text)
      , ("lxc.monitor.unshare", text)
      , ("lxc.mount", text)
      , ("lxc.mount.auto", text)
      , ("lxc.mount.entry", text)
      , ("lxc.network", text)
      , ("lxc.network.", text)
      , ("lxc.network.flags", text)
      , ("lxc.network.hwaddr", text)
      , ("lxc.network.ipv4", text)
      , ("lxc.network.ipv4.gateway", text)
      , ("lxc.network.ipv6", text)
      , ("lxc.network.ipv6.gateway", text)
      , ("lxc.network.link", text)
      , ("lxc.network.macvlan.mode", text)
      , ("lxc.network.mtu", text)
      , ("lxc.network.name", text)
      , ("lxc.network.script.down", text)
      , ("lxc.network.script.up", text)
      , ("lxc.network.type", text)
      , ("lxc.network.veth.pair", text)
      , ("lxc.network.vlan.id", text)
      , ("lxc.pivotdir", text)
      , ("lxc.pts", text)
      , ("lxc.rebootsignal", text)
      , ("lxc.rootfs", text)
      , ("lxc.rootfs.mount", text)
      , ("lxc.rootfs.options", text)
      , ("lxc.se_context", text)
      , ("lxc.seccomp", text)
      , ("lxc.start.auto", text)
      , ("lxc.start.delay", text)
      , ("lxc.start.order", text)
      , ("lxc.stopsignal", text)
      , ("lxc.tty", text)
      , ("lxc.utsname", text)
      ])
 where
  option = xpure (arg (arg (:-)) OptionLine)
                 (\case
                    (OptionLine k v :- r) -> Just (k :- v :- r)
                    otherwise -> Nothing)

whiteSpace :: StringBoomerang r r
whiteSpace = lit " " <> lit "\t"

nonEmptyConfigLine :: StringBoomerang r (ConfigLine :- r)
nonEmptyConfigLine =
     comment
  <> optionLine
 where
  comment = xpure (arg (:-) CommentLine)
                  (\case
                    (CommentLine c :- r) -> Just (c :- r)
                    otherwise -> Nothing)
          . manyl whiteSpace . lit "#" . rList (noneOf "\n")

emptyLine = xpure (\r -> EmptyLine :- r)
                  (\case
                    (EmptyLine :- r) -> Just r
                    otherwise        -> Nothing)
          . manyl whiteSpace

configLines :: StringBoomerang r ([ConfigLine] :- r)
configLines =
  -- to simplify last new line parsing I've split empty line case
     (rCons . nonEmptyConfigLine . (eol . configLines <> push []))
  <> (rCons . emptyLine . eol . configLines <> manyl whiteSpace . push [])

-- This really simple version of modification API
-- keeps only options values and skips the rest
-- configOptions :: StringBoomerang (HashMap LxcOption) r ([ConfigLine] :- r)
-- configOptions = 


-- XXX: migrate to type safty
-- this is really buggy version with just texts as types
-- convert appropriate options to concrete types and define
-- appropriate serialization functions
-- data LxcOption =
--     LxcAaAllowIncompete Text | LxcAaProfile Text | LxcArch Text | LxcAutodev Text
--   | LxcCapDrop Text | LxcCapKeep Text | LxcCgroup Text | LxcConsole Text
--   | LxcConsoleLogfile Text | LxcDevTtyDir Text | LxcEnvironment Text
--   | LxcEphemeral Text | LxcGroup Text | LxcHaltSignal Text
--   | LxcHook Text | LxcHookAutodev Text | LxcHookClone Text | LxcHookDestroy Text
--   | LxcHookMount Text | LxcHookPostStop Text | LxcHookPreMount Text | LxcHookPreStart Text
--   | LxcHookStart Text | LxcHookStop Text | LxcIdMap Text | LxcInclude Text | LxcInitCmd Text
--   | LxcInitGid Text | LxcInitUid Text | LxcKmsg Text | LxcLogfile Text | LxcLoglevel Text
--   | LxcMonitorUnshare Text | LxcMount Text | LxcMountAuto Text | LxcMountEntry Text
--   | LxcNetwork Text |  LxcNetworkFlags Text | LxcNetworkHwaddr Text
--   | LxcNetworkIpv4 Text | LxcNetworkIpv4.gateway Text | LxcNetwork.ipv6 Text
--   | LxcNetwork.ipv6.gateway Text | LxcNetwork.link Text | LxcNetwork.macvlan.mode Text
--   | LxcNetwork.mtu Text | LxcNetwork.name Text | LxcNetwork.script.down Text
--   | LxcNetwork.script.up Text | LxcNetwork.type Text | LxcNetwork.veth.pair Text
--   | LxcNetwork.vlan.id Text | Lxc.pivotdir Text | Lxc.pts Text | Lxc.rebootsignal Text
--   | Lxc.rootfs Text | Lxc.rootfs.mount Text | Lxc.rootfs.options Text | Lxc.se_context Text
--   | Lxc.seccomp Text | Lxc.start.auto Text | Lxc.start.delay Text | Lxc.start.order Text
--   | Lxc.stopsignal Text | Lxc.tty Text | Lxc.utsname Text 
-- option :: StringBoomerang (LxcOption :- r) (String :- r)
-- option =
