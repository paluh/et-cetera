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
import           Text.Boomerang.Prim (Boomerang(..), Parser(..), prs, ser, xpure)
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

data FieldConfig =
    TextField (Lens' LxcConfig (Maybe String))
  | SwitchField (Lens' LxcConfig (Maybe Switch))
  | ListField (Lens' LxcConfig [String])
  | IntField (Lens' LxcConfig (Maybe Int))
  | NetworkTypeField (Lens' LxcConfig (Maybe NetworkType))

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
  Boomerang pf sf
 where
  process =
    xpure (\(v :- lxc :- r) -> over l (v ++ ) lxc :- r)
          (\(lxc :- r) -> let v = view l lxc
                          in if null v
                              then Nothing
                              else Just (v :- lxc :-r))
  -- parse one at a time
  pf = prs (process . rList p)
  -- print all at ones
  sf = ser (process . rList (p . eol))

-- not exactly an isomorphisms ;-)
addOpt :: StringBoomerang (LxcConfig :- r) (LxcConfig :- r)->
          StringBoomerang (LxcConfig :- r) (LxcConfig :- r)->
          StringBoomerang (LxcConfig :- r) (LxcConfig :- r)
addOpt o p =
  Boomerang pf sf
 where
  pf = prs (o <> p)
  sf = ser (((o . lit "\n") <> lit "dupa") . p)

anyOption =
           scalar lxcAaProfileLens (lit "lxc.aa_profile" . lit "=" . value)
  `addOpt` scalar lxcArchLens (lit "lxc.arch" . lit "=" . value)
  `addOpt` vector lxcIncludeLens (lit "lxc.include" . lit "=" . value)
  `addOpt` scalar lxcArchLens (lit "lxc.arch" . lit "=" . value)
  `addOpt` scalar lxcAaProfileLens (lit "lxc.aa_profile" . lit "=" . value)

new =
  Boomerang pf sf
 where
  parserBoomerang = (lit "#" . manyl (ignoreWhen (not . (== '\n')))
                     <> manyl whiteSpace
                     <> anyOption)
                  . opt ((eol . parserBoomerang) <> eol)
  pf = prs parserBoomerang
  sf = ser anyOption

newParse :: String -> Either ParsingError LxcConfig
newParse conf =
  either (Left . ParserError) Right (parseString (new . push emptyConfig) conf)

newSerialize :: LxcConfig -> Either SerializtionError String
newSerialize lxc =
  note SerializtionError . unparseString (new . push lxc) $ lxc

-- not exactly an isomorphism :-)
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

-- fc = FC
--   lxcAaProfileLens
--   ((hdMap Just (lit "lxc.aa_profile" . lit "=" . value)) <> push Nothing)
--  FC lxcArchLens (hdMap Just (lit "lxc.arch" . lit "=" . value) <> push Nothing) :- ()

-- boomerang for parse: Boomerang () a
-- fc2parser :: FC a -> StringBoomerang () LxcConfig -> StringBoomerang () LxcConfig
-- fc2parser (FC l p) b =
--   xpure (\(v :- lxc) -> set l v lxc)
--         (\lxc -> Just (view l lxc :- lxc))
--   . p . b
-- fc2parser (FC l p :- t) r =
--   (xpure (arg (:-) (set l)) undefined . p) <> fc2parser t r

fieldsConfig = HashMap.fromList
  [ ("lxc.aa_allow_incomplete", TextField lxcAaAllowIncompleteLens)
  , ("lxc.aa_profile", TextField lxcAaProfileLens)
  , ("lxc.arch", TextField lxcArchLens)
  , ("lxc.autodev", SwitchField lxcAutodevLens)
  , ("lxc.cap.drop", TextField lxcCapDropLens)
  , ("lxc.cap.keep", TextField lxcCapKeepLens)
  , ("lxc.cgroup", TextField lxcCgroupLens)
  , ("lxc.console", TextField lxcConsoleLens)
  , ("lxc.console.logfile", TextField lxcConsoleLogfileLens)
  , ("lxc.devttydir", TextField lxcDevttydirLens)
  , ("lxc.environment", ListField lxcEnvironmentLens)
  , ("lxc.ephemeral", TextField lxcEphemeralLens)
  , ("lxc.group", TextField lxcGroupLens)
  , ("lxc.haltsignal", TextField lxcHaltsignalLens)
  , ("lxc.hook", TextField lxcHookLens)
  , ("lxc.hook.autodev", TextField lxcHookAutodevLens)
  , ("lxc.hook.clone", TextField lxcHookCloneLens)
  , ("lxc.hook.destroy", TextField lxcHookDestroyLens)
  , ("lxc.hook.mount", TextField lxcHookMountLens)
  , ("lxc.hook.post-stop", TextField lxcHookPostStopLens)
  , ("lxc.hook.pre-mount", TextField lxcHookPreMountLens)
  , ("lxc.hook.pre-start", TextField lxcHookPreStartLens)
  , ("lxc.hook.start", TextField lxcHookStartLens)
  , ("lxc.hook.stop", TextField lxcHookStopLens)
  , ("lxc.id_map", TextField lxcIdMapLens)
  , ("lxc.include", ListField lxcIncludeLens)
  , ("lxc.init_cmd", TextField lxcInitCmdLens)
  , ("lxc.init_gid", TextField lxcInitGidLens)
  , ("lxc.init_uid", TextField lxcInitUidLens)
  , ("lxc.kmsg", SwitchField lxcKmsgLens)
  , ("lxc.logfile", TextField lxcLogfileLens)
  , ("lxc.loglevel", TextField lxcLoglevelLens)
  , ("lxc.monitor.unshare", TextField lxcMonitorUnshareLens)
  , ("lxc.mount", TextField lxcMountLens)
  , ("lxc.mount.auto", TextField lxcMountAutoLens)
  , ("lxc.mount.entry", TextField lxcMountEntryLens)
  , ("lxc.network", TextField lxcNetworkLens)
  , ("lxc.network.flags", TextField lxcNetworkFlagsLens)
  , ("lxc.network.hwaddr", TextField lxcNetworkHwaddrLens)
  , ("lxc.network.ipv4", TextField lxcNetworkIpv4Lens)
  , ("lxc.network.ipv4.gateway", TextField lxcNetworkIpv4GatewayLens)
  , ("lxc.network.ipv6", TextField lxcNetworkIpv6Lens)
  , ("lxc.network.ipv6.gateway", TextField lxcNetworkIpv6GatewayLens)
  , ("lxc.network.link", TextField lxcNetworkLinkLens)
  , ("lxc.network.macvlan.mode", TextField lxcNetworkMacvlanModeLens)
  , ("lxc.network.mtu", TextField lxcNetworkMtuLens)
  , ("lxc.network.name", TextField lxcNetworkNameLens)
  , ("lxc.network.script.down", TextField lxcNetworkScriptDownLens)
  , ("lxc.network.script.up", TextField lxcNetworkScriptUpLens)
  , ("lxc.network.type", NetworkTypeField lxcNetworkTypeLens)
  , ("lxc.network.veth.pair", TextField lxcNetworkVethPairLens)
  , ("lxc.network.vlan.id", TextField lxcNetworkVlanIdLens)
  , ("lxc.pivotdir", TextField lxcPivotdirLens)
  , ("lxc.pts", TextField lxcPtsLens)
  , ("lxc.rebootsignal", TextField lxcRebootsignalLens)
  , ("lxc.rootfs", TextField lxcRootfsLens)
  , ("lxc.rootfs.mount", TextField lxcRootfsMountLens)
  , ("lxc.rootfs.options", TextField lxcRootfsOptionsLens)
  , ("lxc.se_context", TextField lxcSeContextLens)
  , ("lxc.seccomp", TextField lxcSeccompLens)
  , ("lxc.start.auto", SwitchField lxcStartAutoLens)
  , ("lxc.start.delay", IntField lxcStartDelayLens)
  , ("lxc.start.order", IntField lxcStartOrderLens)
  , ("lxc.stopsignal", TextField lxcStopsignalLens)
  , ("lxc.tty", TextField lxcTtyLens)
  , ("lxc.utsname", TextField lxcUtsnameLens)
  ]

type Key = String
data Value = VText String
           | VInt Int
           | VSwitch Switch
           -- used to combine options: environment, include
           | VListOfTextValues [String]
           | VNetworkType NetworkType
  deriving (Eq, Show)

data ConfigLine = EmptyLine | CommentLine String | OptionLine Key Value
  deriving (Eq, Show)

noneOf :: String -> StringBoomerang r (Char :- r)
noneOf l = satisfy (not . (`elem` l))

oneOf :: String -> StringBoomerang r (Char :- r)
oneOf l = satisfy (`elem` l)

eol :: StringBoomerang r r
eol = lit "\n"

word :: String -> StringBoomerang r (String :- r)
word []     = rNil
word (x:xs) = rCons . char x . word xs

value :: StringBoomerang r (String :- r)
value = rList1 (noneOf "\n \t")

optionLine :: StringBoomerang r (ConfigLine :- r)
optionLine =
  foldl' (<>) mempty
    (map (\(l, vp) -> option . word l . manyl whiteSpace
                   .  lit "=" . manyl whiteSpace . vp)
      [(n, fc2prs fc) | (n, fc) <- HashMap.toList fieldsConfig])
 where
  option = xpure (arg (arg (:-)) OptionLine)
                 (\case
                    (OptionLine k v :- r) -> Just (k :- v :- r)
                    otherwise -> Nothing)
  fc2prs (TextField _) = text
  fc2prs (SwitchField _) = switch
  fc2prs (ListField _) = text
  fc2prs (IntField _) = int
  fc2prs (NetworkTypeField _) = networkType

  text :: StringBoomerang r (Value :- r)
  text = xpure (arg (:-) VText)
               (\case
                  (VText t :- r) -> Just (t :- r)
                  otherwise          -> Nothing)
       . value

  switch :: StringBoomerang r (Value :- r)
  switch = xpure (arg (:-) (\case
                              '0' -> VSwitch Off
                              '1' -> VSwitch On))
                 (\case
                    (VSwitch On :- r)  -> Just ('1' :- r)
                    (VSwitch Off :- r) -> Just ('0' :- r)
                    otherwise          -> Nothing)
         . oneOf "01"

  int :: StringBoomerang r (Value :- r)
  int =
    xpure (arg (:-) VInt)
          (\case
            (VInt i :- r) -> Just (i :- r)
            otherwise     -> Nothing)
    . xpure (arg (:-) read)
            (Just . arg (:-) show)
    . rList1 digit

  networkType :: StringBoomerang r (Value :- r)
  networkType =
    xpure (arg (:-) VNetworkType)
          (\case
            (VNetworkType nt :- r) -> Just (nt :- r)
            otherwise              -> Nothing)
    . xpure (arg (:-) (read . capitalize))
          (Just . arg (:-) (map toLower . show))
    . (word "none" <> word  "empty" <> word  "veth"
       <> word  "vlan" <> word  "macvlan" <> word  "phys")

  capitalize :: String -> String
  capitalize [] = []
  capitalize (c:cs) = toUpper c:cs

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

data ParsingError = MultipleOccurencesOfScalarOption String
                  | ParserError StringError
  deriving (Eq, Show)

data SerializtionError = SerializtionError
  deriving (Eq, Show)

serialize :: LxcConfig -> Either SerializtionError String
serialize lxcConf =
  note SerializtionError
    . unparseString configLines
    . foldl' fieldConfig2ConfigLine [] $ HashMap.toList fieldsConfig
 where
  fieldConfig2ConfigLine result (k, TextField l) =
    case view l lxcConf of
      Just v  -> OptionLine k (VText v) : result
      Nothing -> result
  fieldConfig2ConfigLine result (k, SwitchField l) =
    case view l lxcConf of
      Just v  -> OptionLine k (VSwitch v) : result
      Nothing -> result
  fieldConfig2ConfigLine result (k, IntField l) =
    case view l lxcConf of
      Just v  -> OptionLine k (VInt v) : result
      Nothing -> result
  fieldConfig2ConfigLine result (k, NetworkTypeField l) =
    case view l lxcConf of
      Just v  -> OptionLine k (VNetworkType v) : result
      Nothing -> result
  fieldConfig2ConfigLine result (k, ListField l) =
    foldl' (\r v -> OptionLine k (VText v) : r) result (view l lxcConf)

parse :: String -> Either ParsingError LxcConfig
parse conf =
  either (Left . ParserError) lxcConfig confLns
 where
  confLns = parseString configLines conf
  lxcConfig = foldl' setValue (Right emptyConfig)

  setValue :: Either ParsingError LxcConfig -> ConfigLine -> Either ParsingError LxcConfig
  setValue eitherLxcConf confLine = do
    lxcConf <- eitherLxcConf
    case confLine of
      (OptionLine k v) -> case fromJust . HashMap.lookup k $ fieldsConfig of
                            (TextField l)        -> let (VText v') = v
                                                    in setScalarValue lxcConf k v' l
                                  -- use fromJust as all options are parsed based on fieldsConfig
                            (SwitchField l)      -> let (VSwitch v') = v
                                                    in setScalarValue lxcConf k v' l
                            (IntField l)         -> let (VInt v') = v
                                                    in setScalarValue lxcConf k v' l
                            (NetworkTypeField l) -> let (VNetworkType v') = v
                                                    in setScalarValue lxcConf k v' l
                            (ListField l) -> let VText v' = v
                                                in return $ over l (v':) lxcConf
      -- skip comments and empty lines
      otherwise -> eitherLxcConf
   where
    setScalarValue :: LxcConfig -> Key -> a ->
                      Lens' LxcConfig (Maybe a) ->
                      Either ParsingError LxcConfig
    setScalarValue lxcConf k v l =
      if not . null $ view l lxcConf
        then throwError (MultipleOccurencesOfScalarOption k)
        else return $ set l (Just v) lxcConf
