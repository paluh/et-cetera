{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.Internal where

import           Control.Category (id, (.))
import           Control.Lens (Lens', over, set, view)
import           Data.Monoid ((<>))
import           Generics.Deriving.Base (Generic)
import           Generics.Deriving.Monoid (GMonoid, gmempty, gmappend)
import           Prelude hiding (id, (.))
import           Text.Boomerang.Combinators (manyl, manyr, opt, push, rCons, rList, rList1, rListSep, rNil, somel, somer)
import           Text.Boomerang.HStack ((:-)(..))
import           Text.Boomerang.String (StringBoomerang, StringError)
import           System.EtCetera.Internal.Boomerangs (eol)
import           System.EtCetera.Internal.Prim (Prs(..), Ser(..), toPrs, toSer)

-- | Configuration files usually are just sets
-- of unodered options. I wasn't able to easily
-- express final parser and printer constructions
-- with Boomerangs only, so my strategy is following:
--
-- * create separate boomerangs for every single option
--
-- * create separate parser - it will hold sum off all options
--  parsers, so it can parse any option definition
--
-- * create separate serializer which will hold whole config
--  serialization
--
-- * use these options boomerangs with options lens to feed above
--  parser and serializer at once
--
-- * these will produce partial parser (it parses only single option)
--  and final serializer, because they are not isomorphic I've decided
--  to not use Boomerang to hold them


type SingleOptionParser config =
  Prs StringError String (config :- ()) (config :- ())

type Serializer config =
  Ser String (config :- ()) (config :- ())

data Optional a = Present a | Missing
  deriving (Eq, Show)

instance Monoid (Optional a) where
  mempty = Missing
  v@(Present a) `mappend` _ = v
  _           `mappend` u = u

instance GMonoid (Optional a) where
  gmempty = mempty
  gmappend = mappend

extendSerializerWithScalarOption :: Lens' config (Optional a) ->
                                    StringBoomerang (config :- ()) (a :- config :- ()) ->
                                    Serializer config ->
                                    Serializer config
extendSerializerWithScalarOption optionLens optionBoomerang serializer =
  serializer . valueExtractor . optionSerializer <> serializer
 where
  optionSerializer = toSer (optionBoomerang . eol)
  valueExtractor =
    Ser (\(config :- ()) ->
      case view optionLens config of
        (Present v) -> Just (id, v :- config :- ())
        Missing     -> Nothing)

extendSerializerWithVectorOption :: Lens' config [a] ->
                                    StringBoomerang (config :- ()) ([a] :- config :- ()) ->
                                    Serializer config ->
                                    Serializer config
extendSerializerWithVectorOption optionLens optionBoomerang serializer =
  serializer . valueExtractor . optionSerializer <> serializer
 where
  optionSerializer = toSer (optionBoomerang . eol)
  valueExtractor =
    Ser
      (\(config :- ()) ->
        let ov = view optionLens config
        in (case ov of
          [] -> Nothing
          otherwise -> Just (id, ov :- config :- ())))

extendSerializerWithRepeatableScalar :: Lens' config [a] ->
                                    StringBoomerang (config :- ()) (a :- config :- ()) ->
                                    Serializer config ->
                                    Serializer config
extendSerializerWithRepeatableScalar optionLens optionBoomerang serializer =
  serializer . valueExtractor . optionSerializer <> serializer
 where
  optionSerializer = toSer (rList (optionBoomerang . eol))
  valueExtractor =
    Ser (\(config :- r) -> Just (id, view optionLens config :- config :- r))

addScalarOptionParser :: Lens' config (Optional a) ->
                         StringBoomerang (config :- ()) (a :- config :- ()) ->
                         SingleOptionParser config ->
                         SingleOptionParser config
addScalarOptionParser optionLens optionBoomerang optionsParser =
  optionsParser <> (valueSetter . toPrs optionBoomerang)
 where
  valueSetter =
    Prs (return (\(v :- config :- r) -> set optionLens (Present v) config :- r))

addVectorOptionParser :: Lens' config [a] ->
                         StringBoomerang (config :- ()) ([a] :- config :- ()) ->
                         SingleOptionParser config ->
                         SingleOptionParser config
addVectorOptionParser optionLens optionBoomerang optionsParser =
  optionsParser <> (valueSetter . toPrs optionBoomerang)
 where
  valueSetter =
    Prs (return (\(v :- config :- r) -> set optionLens v config :- r))

addRepeatableScalarParser :: Lens' config [a] ->
                             StringBoomerang (config :- ()) (a :- config :- ()) ->
                             SingleOptionParser config ->
                             SingleOptionParser config
addRepeatableScalarParser optionLens optionBoomerang optionsParser =
  (valueSetter . toPrs optionBoomerang) <> optionsParser
 where
  valueSetter =
    Prs (return (\(v :- config :- r) -> over optionLens (v :) config :- r))

vector :: Lens' config [a] ->
          StringBoomerang (config :- ()) ([a] :- config :- ()) ->
          (SingleOptionParser config, Serializer config) ->
          (SingleOptionParser config, Serializer config)
vector optionLens optionBoomerang (p, s) =
  (p', s')
 where
  p' = addVectorOptionParser optionLens optionBoomerang p
  s' = extendSerializerWithVectorOption optionLens optionBoomerang s

scalar :: Lens' config (Optional a) ->
          StringBoomerang (config :- ()) (a :- config :- ()) ->
          (SingleOptionParser config, Serializer config) ->
          (SingleOptionParser config, Serializer config)
scalar optionLens optionBoomerang (p, s) =
  (p', s')
 where
  p' = addScalarOptionParser optionLens optionBoomerang p
  s' = extendSerializerWithScalarOption optionLens optionBoomerang s

repeatableScalar :: Lens' config [a] ->
                    StringBoomerang (config :- ()) (a :- config :- ()) ->
                    (SingleOptionParser config, Serializer config) ->
                    (SingleOptionParser config, Serializer config)
repeatableScalar optionLens optionBoomerang (p, s) =
  (p', s')
 where
  p' = addRepeatableScalarParser optionLens optionBoomerang p
  s' = extendSerializerWithRepeatableScalar optionLens optionBoomerang s

