{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module System.EtCetera.Lxc.Internal where

import           Control.Arrow (first)
import           Control.Category ((.), id)
import           Control.Error (note)
import           Control.Lens (DefName(..), Lens', lensField, lensRules, makeLensesWith,
                               over, set, view, (&), (.~), (??))
import           Control.Monad.Except (MonadError, throwError)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (foldl')
import           Data.Monoid (mempty, (<>))
import           Data.Maybe (fromJust, fromMaybe, Maybe)
import           Data.Char (toLower, toUpper)
import           Language.Haskell.TH (DecsQ, mkName, Name, nameBase)
import           Prelude hiding ((.), id)
import           Text.Boomerang.Combinators (manyl, opt, push, rCons, rList, rList1, rNil)
import           Text.Boomerang.Error (ErrorMsg(..), mkParserError)
import           Text.Boomerang.HStack (arg, hdMap, (:-)(..))
import           Text.Boomerang.Pos (incMajor, incMinor)
import           Text.Boomerang.Prim (Boomerang(..), Parser(..), prs, ser, unparse, xpure)
import           Text.Boomerang.String (anyChar, char, digit, lit, parseString, satisfy,
                                        StringBoomerang, StringError, unparseString)


data Switch = On | Off
  deriving (Eq, Show)

data NetworkType = None | Empty | Veth | Vlan | Macvlan | Phys
  deriving (Eq, Read, Show)

-- options list taken from here:
-- https://github.com/lxc/lxc/blob/ffe344373e5d2b9f2be517f138bf42f9c7d0ca20/src/lxc/confile.c#L116
data LxcConfig =
  LxcConfig
    { lxcAaAllowIncomplete :: Maybe String
    , lxcAaProfile :: Maybe String
    , lxcArch :: Maybe String
    , lxcAutodev :: Maybe Switch
    , lxcCapDrop :: Maybe String
    , lxcCapKeep :: Maybe String
    , lxcCgroup :: Maybe String
    , lxcConsole :: Maybe String
    , lxcConsoleLogfile :: Maybe String
    , lxcDevttydir :: Maybe String
    , lxcEnvironment :: [String]
    , lxcEphemeral :: Maybe String
    , lxcGroup :: Maybe String
    , lxcHaltsignal :: Maybe String
    , lxcHook :: Maybe String
    , lxcHookAutodev :: Maybe String
    , lxcHookClone :: Maybe String
    , lxcHookDestroy :: Maybe String
    , lxcHookMount :: Maybe String
    , lxcHookPostStop :: Maybe String
    , lxcHookPreMount :: Maybe String
    , lxcHookPreStart :: Maybe String
    , lxcHookStart :: Maybe String
    , lxcHookStop :: Maybe String
    , lxcIdMap :: Maybe String
    , lxcInclude :: [String]
    , lxcInitCmd :: Maybe String
    , lxcInitGid :: Maybe String
    , lxcInitUid :: Maybe String
    , lxcKmsg :: Maybe Switch
    , lxcLogfile :: Maybe String
    , lxcLoglevel :: Maybe String
    , lxcMonitorUnshare :: Maybe String
    , lxcMount :: Maybe String
    , lxcMountAuto :: Maybe String
    , lxcMountEntry :: Maybe String
    , lxcNetwork :: Maybe String
    , lxcNetworkFlags :: Maybe String
    , lxcNetworkHwaddr :: Maybe String
    , lxcNetworkIpv4 :: Maybe String
    , lxcNetworkIpv4Gateway :: Maybe String
    , lxcNetworkIpv6 :: Maybe String
    , lxcNetworkIpv6Gateway :: Maybe String
    , lxcNetworkLink :: Maybe String
    , lxcNetworkMacvlanMode :: Maybe String
    , lxcNetworkMtu :: Maybe String
    , lxcNetworkName :: Maybe String
    , lxcNetworkScriptDown :: Maybe String
    , lxcNetworkScriptUp :: Maybe String
    , lxcNetworkType :: Maybe NetworkType
    , lxcNetworkVethPair :: Maybe String
    , lxcNetworkVlanId :: Maybe String
    , lxcPivotdir :: Maybe String
    , lxcPts :: Maybe String
    , lxcRebootsignal :: Maybe String
    , lxcRootfs :: Maybe String
    , lxcRootfsMount :: Maybe String
    , lxcRootfsOptions :: Maybe String
    , lxcSeContext :: Maybe String
    , lxcSeccomp :: Maybe String
    , lxcStartAuto :: Maybe Switch
    , lxcStartDelay :: Maybe Int
    , lxcStartOrder :: Maybe Int
    , lxcStopsignal :: Maybe String
    , lxcTty :: Maybe String
    , lxcUtsname :: Maybe String
    }
  deriving (Eq, Show)

makeLensesWith ?? ''LxcConfig $ lensRules
    & lensField .~ \_ _ name -> [TopName (mkName $ nameBase name ++ "Lens")]

emptyConfig =
  LxcConfig
    { lxcAaAllowIncomplete  = Nothing
    , lxcAaProfile  = Nothing
    , lxcArch  = Nothing
    , lxcAutodev  = Nothing
    , lxcCapDrop  = Nothing
    , lxcCapKeep  = Nothing
    , lxcCgroup  = Nothing
    , lxcConsole  = Nothing
    , lxcConsoleLogfile  = Nothing
    , lxcDevttydir  = Nothing
    , lxcEnvironment = []
    , lxcEphemeral  = Nothing
    , lxcGroup  = Nothing
    , lxcHaltsignal  = Nothing
    , lxcHook  = Nothing
    , lxcHookAutodev  = Nothing
    , lxcHookClone  = Nothing
    , lxcHookDestroy  = Nothing
    , lxcHookMount  = Nothing
    , lxcHookPostStop  = Nothing
    , lxcHookPreMount  = Nothing
    , lxcHookPreStart  = Nothing
    , lxcHookStart  = Nothing
    , lxcHookStop  = Nothing
    , lxcIdMap  = Nothing
    , lxcInclude = []
    , lxcInitCmd  = Nothing
    , lxcInitGid  = Nothing
    , lxcInitUid  = Nothing
    , lxcKmsg  = Nothing
    , lxcLogfile  = Nothing
    , lxcLoglevel  = Nothing
    , lxcMonitorUnshare  = Nothing
    , lxcMount  = Nothing
    , lxcMountAuto  = Nothing
    , lxcMountEntry  = Nothing
    , lxcNetwork  = Nothing
    , lxcNetworkFlags  = Nothing
    , lxcNetworkHwaddr  = Nothing
    , lxcNetworkIpv4  = Nothing
    , lxcNetworkIpv4Gateway  = Nothing
    , lxcNetworkIpv6  = Nothing
    , lxcNetworkIpv6Gateway  = Nothing
    , lxcNetworkLink  = Nothing
    , lxcNetworkMacvlanMode  = Nothing
    , lxcNetworkMtu  = Nothing
    , lxcNetworkName  = Nothing
    , lxcNetworkScriptDown  = Nothing
    , lxcNetworkScriptUp  = Nothing
    , lxcNetworkType  = Nothing
    , lxcNetworkVethPair  = Nothing
    , lxcNetworkVlanId  = Nothing
    , lxcPivotdir  = Nothing
    , lxcPts  = Nothing
    , lxcRebootsignal  = Nothing
    , lxcRootfs  = Nothing
    , lxcRootfsMount  = Nothing
    , lxcRootfsOptions  = Nothing
    , lxcSeContext  = Nothing
    , lxcSeccomp  = Nothing
    , lxcStartAuto  = Nothing
    , lxcStartDelay  = Nothing
    , lxcStartOrder  = Nothing
    , lxcStopsignal  = Nothing
    , lxcTty  = Nothing
    , lxcUtsname  = Nothing
    }

lxcConfig =
  Boomerang pf sf
 where
  -- XXX: nearly all of below boomerangs
  --      are not "isomorpisms" at all.
  --      It is just easier to build
  --      serializer and parser using boomerang's
  --      combinators.
  parserBoomerang = (lit "#" . manyl (ignoreWhen (not . (== '\n')))
                     <> manyl whiteSpace
                     <> anyOption)
                  . opt ((eol . parserBoomerang) <> eol)
  pf = prs parserBoomerang
  sf = ser anyOption

  anyOption =
             scalar lxcAaAllowIncompleteLens (option "lxc.aa_allow_incomplete" text)
    `addOpt` scalar lxcAaProfileLens (option "lxc.aa_profile" text)
    `addOpt` scalar lxcArchLens (option "lxc.arch" text)
    `addOpt` scalar lxcAutodevLens (option "lxc.autodev" switch)
    `addOpt` scalar lxcCapDropLens (option "lxc.cap.drop" text)
    `addOpt` scalar lxcCapKeepLens (option "lxc.cap.keep" text)
    `addOpt` scalar lxcCgroupLens (option "lxc.cgroup" text)
    `addOpt` scalar lxcConsoleLens (option "lxc.console" text)
    `addOpt` scalar lxcConsoleLogfileLens (option "lxc.console.logfile" text)
    `addOpt` scalar lxcDevttydirLens (option "lxc.devttydir" text)
    `addOpt` vector lxcEnvironmentLens (option "lxc.environment" text)
    `addOpt` scalar lxcEphemeralLens (option "lxc.ephemeral" text)
    `addOpt` scalar lxcGroupLens (option "lxc.group" text)
    `addOpt` scalar lxcHaltsignalLens (option "lxc.haltsignal" text)
    `addOpt` scalar lxcHookLens (option "lxc.hook" text)
    `addOpt` scalar lxcHookAutodevLens (option "lxc.hook.autodev" text)
    `addOpt` scalar lxcHookCloneLens (option "lxc.hook.clone" text)
    `addOpt` scalar lxcHookDestroyLens (option "lxc.hook.destroy" text)
    `addOpt` scalar lxcHookMountLens (option "lxc.hook.mount" text)
    `addOpt` scalar lxcHookPostStopLens (option "lxc.hook.post-stop" text)
    `addOpt` scalar lxcHookPreMountLens (option "lxc.hook.pre-mount" text)
    `addOpt` scalar lxcHookPreStartLens (option "lxc.hook.pre-start" text)
    `addOpt` scalar lxcHookStartLens (option "lxc.hook.start" text)
    `addOpt` scalar lxcHookStopLens (option "lxc.hook.stop" text)
    `addOpt` scalar lxcIdMapLens (option "lxc.id_map" text)
    `addOpt` vector lxcIncludeLens (option "lxc.include" text)
    `addOpt` scalar lxcInitCmdLens (option "lxc.init_cmd" text)
    `addOpt` scalar lxcInitGidLens (option "lxc.init_gid" text)
    `addOpt` scalar lxcInitUidLens (option "lxc.init_uid" text)
    `addOpt` scalar lxcKmsgLens (option "lxc.kmsg" switch)
    `addOpt` scalar lxcLogfileLens (option "lxc.logfile" text)
    `addOpt` scalar lxcLoglevelLens (option "lxc.loglevel" text)
    `addOpt` scalar lxcMonitorUnshareLens (option "lxc.monitor.unshare" text)
    `addOpt` scalar lxcMountLens (option "lxc.mount" text)
    `addOpt` scalar lxcMountAutoLens (option "lxc.mount.auto" text)
    `addOpt` scalar lxcMountEntryLens (option "lxc.mount.entry" text)
    `addOpt` scalar lxcNetworkLens (option "lxc.network" text)
    `addOpt` scalar lxcNetworkFlagsLens (option "lxc.network.flags" text)
    `addOpt` scalar lxcNetworkHwaddrLens (option "lxc.network.hwaddr" text)
    `addOpt` scalar lxcNetworkIpv4Lens (option "lxc.network.ipv4" text)
    `addOpt` scalar lxcNetworkIpv4GatewayLens (option "lxc.network.ipv4.gateway" text)
    `addOpt` scalar lxcNetworkIpv6Lens (option "lxc.network.ipv6" text)
    `addOpt` scalar lxcNetworkIpv6GatewayLens (option "lxc.network.ipv6.gateway" text)
    `addOpt` scalar lxcNetworkLinkLens (option "lxc.network.link" text)
    `addOpt` scalar lxcNetworkMacvlanModeLens (option "lxc.network.macvlan.mode" text)
    `addOpt` scalar lxcNetworkMtuLens (option "lxc.network.mtu" text)
    `addOpt` scalar lxcNetworkNameLens (option "lxc.network.name" text)
    `addOpt` scalar lxcNetworkScriptDownLens (option "lxc.network.script.down" text)
    `addOpt` scalar lxcNetworkScriptUpLens (option "lxc.network.script.up" text)
    `addOpt` scalar lxcNetworkTypeLens (option "lxc.network.type" networkType)
    `addOpt` scalar lxcNetworkVethPairLens (option "lxc.network.veth.pair" text)
    `addOpt` scalar lxcNetworkVlanIdLens (option "lxc.network.vlan.id" text)
    `addOpt` scalar lxcPivotdirLens (option "lxc.pivotdir" text)
    `addOpt` scalar lxcPtsLens (option "lxc.pts" text)
    `addOpt` scalar lxcRebootsignalLens (option "lxc.rebootsignal" text)
    `addOpt` scalar lxcRootfsLens (option "lxc.rootfs" text)
    `addOpt` scalar lxcRootfsMountLens (option "lxc.rootfs.mount" text)
    `addOpt` scalar lxcRootfsOptionsLens (option "lxc.rootfs.optionions" text)
    `addOpt` scalar lxcSeContextLens (option "lxc.se_context" text)
    `addOpt` scalar lxcSeccompLens (option "lxc.seccomp" text)
    `addOpt` scalar lxcStartAutoLens (option "lxc.start.auto" switch)
    `addOpt` scalar lxcStartDelayLens (option "lxc.start.delay" int)
    `addOpt` scalar lxcStartOrderLens (option "lxc.start.order" int)
    `addOpt` scalar lxcStopsignalLens (option "lxc.stopsignal" text)
    `addOpt` scalar lxcTtyLens (option "lxc.tty" text)
    `addOpt` scalar lxcUtsnameLens (option "lxc.utsname" text)

  addOpt :: StringBoomerang (LxcConfig :- r) (LxcConfig :- r)->
            StringBoomerang (LxcConfig :- r) (LxcConfig :- r)->
            StringBoomerang (LxcConfig :- r) (LxcConfig :- r)
  addOpt p o =
    Boomerang (prs (o <> p)) sf
   where
    psf = ser p
    osf = ser o
    sf s = case osf s of
            [] -> psf s
            fr@((f, s'):_) -> case psf s' of
                                [] -> fr
                                l -> [(g . ('\n':) . f, s'') | (g, s'') <- l]

  scalar :: Lens' LxcConfig (Maybe a) ->
            StringBoomerang (LxcConfig :- ()) (a :- LxcConfig :- ()) ->
            StringBoomerang (LxcConfig :- ()) (LxcConfig :- ())
  scalar l p =
    xpure (\(v :- lxc :- r) -> set l v lxc :- r)
          (\(lxc :- r) -> Just (view l lxc :- lxc :- r))
    . xpure (hdMap Just)
            (\case
              Just v :- r -> Just (v :- r)
              Nothing :- r -> Nothing)
    . p

  vector :: Lens' LxcConfig [a] ->
            StringBoomerang (LxcConfig :- ()) (a :- LxcConfig :- ()) ->
            StringBoomerang (LxcConfig :- ()) (LxcConfig :- ())
  vector l p =
    Boomerang vpf vsf
   where
    -- parse one at a time
    vpf = prs (process . rList p)
    -- print all at ones
    vsf = ser (process . rList (p . eol))
    process =
      xpure (\(v :- lxc :- r) -> over l (v ++ ) lxc :- r)
            (\(lxc :- r) -> let v = view l lxc
                            in if null v
                                then Nothing
                                else Just (v :- lxc :-r))

  -- construct option line boomerang
  option l vp = lit l . manyl whiteSpace .  lit "=" . manyl whiteSpace . vp

  text :: StringBoomerang r (String :- r)
  text = rList1 (noneOf "\n \t")

  switch = xpure (arg (:-) (\case
                              '0' -> Off
                              '1' -> On))
                 (\case
                    (On :- r)  -> Just ('1' :- r)
                    (Off :- r) -> Just ('0' :- r))
         . oneOf "01"
  int =
      xpure (arg (:-) read)
            (Just . arg (:-) show)
    . rList1 digit

  networkType =
      xpure (arg (:-) (read . capitalize))
          (Just . arg (:-) (map toLower . show))
      . (word "none" <> word  "empty" <> word  "veth"
         <> word  "vlan" <> word  "macvlan" <> word  "phys")

  capitalize :: String -> String
  capitalize [] = []
  capitalize (c:cs) = toUpper c:cs


ignoreWhen :: (Char -> Bool) -> StringBoomerang r r
ignoreWhen p = Boomerang
  (Parser $ \tok pos ->
       case tok of
         []        -> mkParserError pos [EOI "input"]
         (c:cs)
             | p c ->
                 [Right ((id, cs),
                          if c == '\n'
                            then incMajor 1 pos
                            else incMinor 1 pos)]
             | otherwise ->
                 mkParserError pos [SysUnExpect $ show c]
  )
  (\r -> [(id, r)])

oneOf :: String -> StringBoomerang r (Char :- r)
oneOf l = satisfy (`elem` l)

noneOf :: String -> StringBoomerang r (Char :- r)
noneOf l = satisfy (not . (`elem` l))

word :: String -> StringBoomerang r (String :- r)
word []     = rNil
word (x:xs) = rCons . char x . word xs

eol :: StringBoomerang r r
eol = lit "\n"

whiteSpace :: StringBoomerang r r
whiteSpace = lit " " <> lit "\t"

data ParsingError = MultipleOccurencesOfScalarOption String
                  | ParserError StringError
  deriving (Eq, Show)

data SerializtionError = SerializtionError
  deriving (Eq, Show)

parse :: String -> Either ParsingError LxcConfig
parse conf =
  either (Left . ParserError) Right (parseString (lxcConfig . push emptyConfig) conf)

serialize :: LxcConfig -> Either SerializtionError String
serialize lxc =
  note SerializtionError . unparseString (lxcConfig . push lxc) $ lxc
