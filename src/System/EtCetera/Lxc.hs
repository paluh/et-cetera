{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.Lxc where

import           Control.Arrow (first)
import           Control.Category ((.), id)
import           Control.Monad.Except (MonadError, throwError)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (foldl')
import           Data.Monoid (mempty, (<>))
import           Prelude hiding ((.))
import           Text.Boomerang.Combinators (manyl, opt, push, rCons, rList, rList1, rNil)
import           Text.Boomerang.HStack (arg, (:-)(..))
import           Text.Boomerang.Prim (xpure)
import           Text.Boomerang.String (anyChar, char, lit, parseString,
                                        satisfy, StringBoomerang, StringError)

type Key = String
data Switch = On | Off
  deriving (Eq, Show)
data Value = VText String
           | VInt Int
           | VSwitch Switch
           -- used to combine options: environment, include
           | VListOfTextValues [String]
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

-- options list taken from here:
-- https://github.com/lxc/lxc/blob/ffe344373e5d2b9f2be517f138bf42f9c7d0ca20/src/lxc/confile.c#L116
optionLine :: StringBoomerang r (ConfigLine :- r)
optionLine =
  foldl' (<>) mempty
    (map (\(l, vp) -> option . word l . manyl whiteSpace
                   .  lit "=" . manyl whiteSpace . vp)
      [ ("lxc.aa_allow_incomplete", text)
      , ("lxc.aa_profile", text)
      , ("lxc.arch", text)
      , ("lxc.autodev", switch)
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
      , ("lxc.kmsg", switch)
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

-- serToOptionList :: HashMap.HashMap String (OptionValue) -> [ConfigLine]
-- serToOptionList = undefined

data ParseError = MultipleOccurencesOfScalarOption String
                | ParserError StringError
  deriving (Eq, Show)

-- convert list of config lines into hash map of options
-- some options are combined in really ugly manner ;-)
prsToOptionMap ::  [ConfigLine] -> Either ParseError (HashMap.HashMap Key Value)
prsToOptionMap =
  foldl' addOption (Right HashMap.empty)
 where
  addOption :: Either ParseError (HashMap.HashMap Key Value) ->
               ConfigLine -> Either ParseError (HashMap.HashMap Key Value)
  addOption ehm cl = do
    hm <- ehm
    case cl of
      (OptionLine k v) | k `HashMap.member` hm && not (isListOfValues k)
                          -> throwError (MultipleOccurencesOfScalarOption k)
                       | k `HashMap.member` hm && isListOfValues k -> do
                          let (VText t) = v
                          return (HashMap.adjust (\(VListOfTextValues l) -> VListOfTextValues (t:l)) k hm)
                       | isListOfValues k -> do
                          let (VText t) = v
                          return (HashMap.insert k  (VListOfTextValues [t]) hm)
                       | otherwise -> return (HashMap.insert k v hm)
      otherwise -> ehm
  isListOfValues :: Key -> Bool
  isListOfValues k = k `elem` ["lxc.include", "lxc.environment"]

parseConfig :: String -> Either ParseError (HashMap.HashMap Key Value)
parseConfig conf =
  let ep = parseString configLines conf
  in either (Left . ParserError) prsToOptionMap ep
