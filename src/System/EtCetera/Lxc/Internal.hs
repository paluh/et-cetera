{-# LANGUAGE DeriveGeneric #-}
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
import           System.EtCetera.Internal (extendSerializerWithScalarOption, Optional(..), repeatableScalar, scalar, Serializer, vector)
import           Generics.Deriving.Base (Generic)
import           Generics.Deriving.Monoid (gmappend, gmempty, GMonoid)
import           Language.Haskell.TH (DecsQ, mkName, Name, nameBase)
import           Prelude hiding ((.), id)
import           Text.Boomerang.Combinators (manyl, manyr, opt, push, rCons, rList, rList1, rNil, somel)
import           Text.Boomerang.Error (ErrorMsg(..), mkParserError)
import           Text.Boomerang.HStack (arg, hdMap, (:-)(..))
import           Text.Boomerang.Pos (incMajor, incMinor)
import           Text.Boomerang.Prim (Boomerang(..), Parser(..), prs, ser, unparse, xpure)
import           Text.Boomerang.String (anyChar, char, digit, lit, satisfy,
                                        StringBoomerang, StringError)
import           System.EtCetera.Internal (SingleOptionParser)
import           System.EtCetera.Internal.Prim (Prs(..), purePrs, Ser(..),
                                                StringPrs, toPrs)
import           System.EtCetera.Internal.Boomerangs (ignoreWhen, noneOf, oneOf, parseString,
                                                      whiteSpace, word, (<?>))

data Switch = On | Off
  deriving (Eq, Show)

data NetworkType = None | Empty | Veth | Vlan | Macvlan | Phys
  deriving (Eq, Read, Show)


data Network =
  Network
    { lxcNetworkType :: Optional NetworkType
    , lxcNetworkFlags :: Optional String
    , lxcNetworkHwaddr :: Optional String
    , lxcNetworkIpv4 :: Optional String
    , lxcNetworkIpv4Gateway :: Optional String
    , lxcNetworkIpv6 :: Optional String
    , lxcNetworkIpv6Gateway :: Optional String
    , lxcNetworkLink :: Optional String
    , lxcNetworkMacvlanMode :: Optional String
    , lxcNetworkMtu :: Optional String
    , lxcNetworkName :: Optional String
    , lxcNetworkScriptDown :: Optional String
    , lxcNetworkScriptUp :: Optional String
    , lxcNetworkVethPair :: Optional String
    , lxcNetworkVlanId :: Optional String
    }
  deriving (Eq, Generic, Show)

instance GMonoid Network
instance Monoid Network where
  mempty = gmempty
  mappend = gmappend

emptyNetwork :: Network
emptyNetwork = mempty

makeLensesWith ?? ''Network $ lensRules
    & lensField .~ \_ _ name -> [TopName (mkName $ nameBase name ++ "Lens")]

-- options list taken from here:
-- https://github.com/lxc/lxc/blob/ffe344373e5d2b9f2be517f138bf42f9c7d0ca20/src/lxc/confile.c#L116
data LxcConfig =
  LxcConfig
    { lxcAaAllowIncomplete :: Optional String
    , lxcAaProfile :: Optional String
    , lxcArch :: Optional String
    , lxcAutodev :: Optional Switch
    , lxcCapDrop :: Optional String
    , lxcCapKeep :: Optional String
    , lxcCgroup :: Optional String
    , lxcConsole :: Optional String
    , lxcConsoleLogfile :: Optional String
    , lxcDevttydir :: Optional String
    , lxcEnvironment :: [String]
    , lxcEphemeral :: Optional String
    , lxcGroup :: Optional String
    , lxcHaltsignal :: Optional String
    , lxcHook :: Optional String
    , lxcHookAutodev :: Optional String
    , lxcHookClone :: Optional String
    , lxcHookDestroy :: Optional String
    , lxcHookMount :: Optional String
    , lxcHookPostStop :: Optional String
    , lxcHookPreMount :: Optional String
    , lxcHookPreStart :: Optional String
    , lxcHookStart :: Optional String
    , lxcHookStop :: Optional String
    , lxcIdMap :: Optional String
    , lxcInclude :: [String]
    , lxcInitCmd :: Optional String
    , lxcInitGid :: Optional String
    , lxcInitUid :: Optional String
    , lxcKmsg :: Optional Switch
    , lxcLogfile :: Optional String
    , lxcLoglevel :: Optional String
    , lxcMonitorUnshare :: Optional String
    , lxcMount :: Optional String
    , lxcMountAuto :: Optional String
    , lxcMountEntry :: Optional String
    , lxcNetwork :: [Network]
    , lxcPivotdir :: Optional String
    , lxcPts :: Optional Int
    , lxcRebootsignal :: Optional String
    , lxcRootfs :: Optional String
    , lxcRootfsMount :: Optional String
    , lxcRootfsOptions :: Optional String
    , lxcSeConstring :: Optional String
    , lxcSeccomp :: Optional String
    , lxcStartAuto :: Optional Switch
    , lxcStartDelay :: Optional Int
    , lxcStartOrder :: Optional Int
    , lxcStopsignal :: Optional String
    , lxcTty :: Optional String
    , lxcUtsname :: Optional String
    }
  deriving (Eq, Generic, Show)

makeLensesWith ?? ''LxcConfig $ lensRules
    & lensField .~ \_ _ name -> [TopName (mkName $ nameBase name ++ "Lens")]

instance GMonoid LxcConfig
instance Monoid LxcConfig where
  mempty = gmempty
  mappend = gmappend

emptyConfig :: LxcConfig
emptyConfig = mempty

option l vp = lit l . opt ws .  lit "=" . opt ws . vp

string :: StringBoomerang r (String :- r)
string = rList1 (noneOf "\n \t")

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

networkType :: forall r. StringBoomerang r (NetworkType :- r)
networkType =
    xpure (arg (:-) (read . capitalize))
        (Just . arg (:-) (map toLower . show))
    . (word "none" <> word  "empty" <> word  "veth"
       <> word  "vlan" <> word  "macvlan" <> word  "phys")

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c:cs

eol :: StringBoomerang r r
eol = lit "\n"

(baseOptionsParser, baseOptionsSerializer) =
  scalar lxcAaAllowIncompleteLens (option "lxc.aa_allow_incomplete" string) .
  scalar lxcAaProfileLens (option "lxc.aa_profile" string) .
  scalar lxcArchLens (option "lxc.arch" string) .
  scalar lxcAutodevLens (option "lxc.autodev" switch) .
  scalar lxcCapDropLens (option "lxc.cap.drop" string) .
  scalar lxcCapKeepLens (option "lxc.cap.keep" string) .
  scalar lxcCgroupLens (option "lxc.cgroup" string) .
  scalar lxcConsoleLens (option "lxc.console" string) .
  scalar lxcConsoleLogfileLens (option "lxc.console.logfile" string) .
  scalar lxcDevttydirLens (option "lxc.devttydir" string) .
  repeatableScalar lxcEnvironmentLens (option "lxc.environment" string) .
  scalar lxcEphemeralLens (option "lxc.ephemeral" string) .
  scalar lxcGroupLens (option "lxc.group" string) .
  scalar lxcHaltsignalLens (option "lxc.haltsignal" string) .
  scalar lxcHookLens (option "lxc.hook" string) .
  scalar lxcHookAutodevLens (option "lxc.hook.autodev" string) .
  scalar lxcHookCloneLens (option "lxc.hook.clone" string) .
  scalar lxcHookDestroyLens (option "lxc.hook.destroy" string) .
  scalar lxcHookMountLens (option "lxc.hook.mount" string) .
  scalar lxcHookPostStopLens (option "lxc.hook.post-stop" string) .
  scalar lxcHookPreMountLens (option "lxc.hook.pre-mount" string) .
  scalar lxcHookPreStartLens (option "lxc.hook.pre-start" string) .
  scalar lxcHookStartLens (option "lxc.hook.start" string) .
  scalar lxcHookStopLens (option "lxc.hook.stop" string) .
  scalar lxcIdMapLens (option "lxc.id_map" string) .
  repeatableScalar lxcIncludeLens (option "lxc.include" string) .
  scalar lxcInitCmdLens (option "lxc.init_cmd" string) .
  scalar lxcInitGidLens (option "lxc.init_gid" string) .
  scalar lxcInitUidLens (option "lxc.init_uid" string) .
  scalar lxcKmsgLens (option "lxc.kmsg" switch) .
  scalar lxcLogfileLens (option "lxc.logfile" string) .
  scalar lxcLoglevelLens (option "lxc.loglevel" string) .
  scalar lxcMonitorUnshareLens (option "lxc.monitor.unshare" string) .
  scalar lxcMountLens (option "lxc.mount" string) .
  scalar lxcMountAutoLens (option "lxc.mount.auto" string) .
  scalar lxcMountEntryLens (option "lxc.mount.entry" string) .
  scalar lxcPivotdirLens (option "lxc.pivotdir" string) .
  scalar lxcPtsLens (option "lxc.pts" int) .
  scalar lxcRebootsignalLens (option "lxc.rebootsignal" string) .
  scalar lxcRootfsLens (option "lxc.rootfs" string) .
  scalar lxcRootfsMountLens (option "lxc.rootfs.mount" string) .
  scalar lxcRootfsOptionsLens (option "lxc.rootfs.optionions" string) .
  scalar lxcSeConstringLens (option "lxc.se_constring" string) .
  scalar lxcSeccompLens (option "lxc.seccomp" string) .
  scalar lxcStartAutoLens (option "lxc.start.auto" switch) .
  scalar lxcStartDelayLens (option "lxc.start.delay" int) .
  scalar lxcStartOrderLens (option "lxc.start.order" int) .
  scalar lxcStopsignalLens (option "lxc.stopsignal" string) .
  scalar lxcTtyLens (option "lxc.tty" string) .
  scalar lxcUtsnameLens (option "lxc.utsname" string)
  $ (mempty, id)

ws = somel whiteSpace

(anyOptionParser, networkSerializer) =
  nScalar lxcNetworkTypeLens (nOption "lxc.network.type" networkType) .
  nScalar lxcNetworkFlagsLens (nOption "lxc.network.flags" string) .
  nScalar lxcNetworkHwaddrLens (nOption "lxc.network.hwaddr" string) .
  nScalar lxcNetworkIpv4Lens (nOption "lxc.network.ipv4" string) .
  nScalar lxcNetworkIpv4GatewayLens (nOption "lxc.network.ipv4.gateway" string) .
  nScalar lxcNetworkIpv6Lens (nOption "lxc.network.ipv6" string) .
  nScalar lxcNetworkIpv6GatewayLens (nOption "lxc.network.ipv6.gateway" string) .
  nScalar lxcNetworkLinkLens (nOption "lxc.network.link" string) .
  nScalar lxcNetworkMacvlanModeLens (nOption "lxc.network.macvlan.mode" string) .
  nScalar lxcNetworkMtuLens (nOption "lxc.network.mtu" string) .
  nScalar lxcNetworkNameLens (nOption "lxc.network.name" string) .
  nScalar lxcNetworkScriptDownLens (nOption "lxc.network.script.down" string) .
  nScalar lxcNetworkScriptUpLens (nOption "lxc.network.script.up" string) .
  nScalar lxcNetworkVethPairLens (nOption "lxc.network.veth.pair" string) .
  nScalar lxcNetworkVlanIdLens (nOption "lxc.network.vlan.id" string)
  $ (baseOptionsParser, id)
 where
  nScalar :: Lens' Network (Optional a) ->
             (forall r. StringBoomerang r (a :- r)) ->
             (SingleOptionParser LxcConfig, Serializer Network) ->
             (SingleOptionParser LxcConfig, Serializer Network)
  nScalar lens ob (prs, ser) =
    (purePrs (\(value :- lxc :- r) -> over lxcNetworkLens (setValue lens value) lxc :- r) . toPrs ob <> prs,
     extendSerializerWithScalarOption lens ob ser)

  nOption l b = lit l . opt ws . lit "=" . opt ws . b

  setValue :: Lens' Network (Optional a) -> a -> [Network] -> [Network]
  -- a little bit cheating here ;-)
  setValue lens value [] = [set lens (Present value) emptyNetwork]
  setValue lens value (n:ns) = set lens (Present value) n : ns

pureSer :: (b -> Maybe a) -> Ser String a b
pureSer f =
  Ser (fmap ((,) id) <$> f)

-- instance (Ser) Category where
--   (Ser f) . (Ser g) =
--     Ser (\a -> do
--       (t2t, b) <- g a
--       (t2t', c) <- f b
--       return (t2t' . t2t, c)
pop :: Ser String r (a :- r)
pop = pureSer (\(a :- r) -> Just r)

networks :: Ser String (LxcConfig :- r) (LxcConfig :- r)
networks =
  -- networksSer . (pureSer p)
  undefined
 where
  p (lxcConfig@(LxcConfig{lxcNetwork=ns}) :- r) = Just (id, ns :- lxcConfig :- r)
  s = Ser p
  networksSer =
    Ser (l (networkSerializer . pop))
   where
    l (Ser s) ([] :- r) = Just (id, r)
    l (Ser s) ((x:xs) :- r) = do
      (t2t, r') <- s (x :- r)
      (t2t', r'') <- l (Ser s) (xs :- r')
      return (t2t' . t2t, r'')

parser =
  -- comment
  ((toPrs (lit "#" . manyr (ignoreWhen (/= '\n')))
  -- only white characters line
   <> toPrs ws
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

data ParsingError = MultipleOccurencesOfScalarOption String
                  | ParserError StringError
  deriving (Eq, Show)

data SerializtionError = SerializtionError
  deriving (Eq, Show)

parse :: String -> Either ParsingError LxcConfig
parse conf =
  either (Left . ParserError) Right (parseString (parser . toPrs (push emptyConfig)) conf)

serialize :: LxcConfig -> Either SerializtionError String
serialize redisConfig =
  note SerializtionError . fmap (($ "") . fst) . s $ (redisConfig :- ())
 where
  Ser s = baseOptionsSerializer
