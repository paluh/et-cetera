{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.Lxc where

import           Control.Category ((.), id)
import           Data.List (foldl')
import           Data.Monoid (mempty, (<>))
import           Prelude hiding ((.))
import           Text.Boomerang.Combinators (manyl, opt, push, rCons, rList, rList1, rNil)
import           Text.Boomerang.HStack (arg, (:-)(..))
import           Text.Boomerang.Prim (xpure)
import           Text.Boomerang.String (anyChar, char, lit, satisfy, StringBoomerang)

type Key = String
type Value = String
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

-- keys list taken from here:
-- https://github.com/lxc/lxc/blob/ffe344373e5d2b9f2be517f138bf42f9c7d0ca20/src/lxc/confile.c#L116
key :: StringBoomerang r (String :- r)
key = foldl' (<>) mempty (map word
      [ "lxc.aa_allow_incomplete", "lxc.aa_profile", "lxc.arch", "lxc.autodev"
      , "lxc.cap.drop", "lxc.cap.keep", "lxc.cgroup", "lxc.console", "lxc.console.logfile"
      , "lxc.devttydir", "lxc.environment", "lxc.ephemeral", "lxc.group", "lxc.haltsignal"
      , "lxc.hook", "lxc.hook.autodev", "lxc.hook.clone", "lxc.hook.destroy"
      , "lxc.hook.mount", "lxc.hook.post-stop", "lxc.hook.pre-mount", "lxc.hook.pre-start"
      , "lxc.hook.start", "lxc.hook.stop", "lxc.id_map", "lxc.include", "lxc.init_cmd"
      , "lxc.init_gid", "lxc.init_uid", "lxc.kmsg", "lxc.logfile", "lxc.loglevel"
      , "lxc.monitor.unshare", "lxc.mount", "lxc.mount.auto", "lxc.mount.entry"
      , "lxc.network", "lxc.network.", "lxc.network.flags", "lxc.network.hwaddr"
      , "lxc.network.ipv4", "lxc.network.ipv4.gateway", "lxc.network.ipv6"
      , "lxc.network.ipv6.gateway", "lxc.network.link", "lxc.network.macvlan.mode"
      , "lxc.network.mtu", "lxc.network.name", "lxc.network.script.down"
      , "lxc.network.script.up", "lxc.network.type", "lxc.network.veth.pair"
      , "lxc.network.vlan.id", "lxc.pivotdir", "lxc.pts", "lxc.rebootsignal"
      , "lxc.rootfs", "lxc.rootfs.mount", "lxc.rootfs.options", "lxc.se_context"
      , "lxc.seccomp", "lxc.start.auto", "lxc.start.delay", "lxc.start.order"
      , "lxc.stopsignal", "lxc.tty", "lxc.utsname"
      ])

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



whiteSpace :: StringBoomerang r r
whiteSpace = lit " " <> lit "\t"

nonEmptyConfigLine :: StringBoomerang r (ConfigLine :- r)
nonEmptyConfigLine =
     comment
  <> (option . key . manyl whiteSpace . lit "=" . manyl whiteSpace . value)
 where
  option = xpure (arg (arg (:-)) OptionLine)
                 (\case
                    (OptionLine k v :- r) -> Just (k :- v :- r)
                    otherwise -> Nothing)
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



