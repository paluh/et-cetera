{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module System.EtCetera.Internal.Prim where

import           Control.Category (Category, id, (.))
import           Control.Monad (mplus, mzero)
import           Data.Maybe (listToMaybe)
import           Data.Monoid (mempty, mappend)
import           Prelude hiding (id, (.))
import           Text.Boomerang.HStack ((:-)(..))
import           Text.Boomerang.Prim (bestErrors, Boomerang(..), Parser(..), prs, runParser, ser)
import qualified Text.Boomerang.Prim
import           Text.Boomerang.Pos (ErrorPosition, initialPos, InitialPosition, Pos)

-- | Sometimes it is mutch easier to separately work
-- on parser and serializer. These types allows to
-- easily split boomerangs and use pices easily.

data Prs e tok a b =
  Prs { runPrs :: Parser e tok (a -> b)}
  deriving (Functor)

instance (a ~ b) => Monoid (Prs e tok a b) where
  mempty = Prs mzero
  ~(Prs pf) `mappend` ~(Prs pg) =
    Prs (pf `mplus` pg)

instance Category (Prs e tok) where
  id = Prs $ return id
  (Prs mf) . (Prs mg) = Prs $ do
    f <- mf
    g <- mg
    return (f . g)

data Ser tok a b =
  Ser { runSer :: (b -> Maybe (tok -> tok, a)) }

instance Monoid (Ser tok a b) where
  mempty = Ser (const mzero)
  (Ser sf) `mappend` (Ser sg) =
    Ser (\s -> sf s `mplus` sg s)

instance Category (Ser tok) where
  id = Ser (\x -> Just (id, x))
  (Ser f) . (Ser g) =
    Ser (\c -> do
      (ft', b) <- f c
      (gt', a) <- g b
      return (ft' . gt', a))

toPrs :: Boomerang e tok a b -> Prs e tok a b
toPrs = Prs . prs

toSer :: Boomerang e tok a b -> Ser tok a b
toSer = Ser . fmap listToMaybe . ser

-- ugly hacks to use predifined parsing function
parse p =
  Text.Boomerang.Prim.parse
    (Boomerang
      (runPrs p)
      (error "Text.Boomerang.EtCetera.Prim.parse: Boomerang.Prim.parse evaluated serializer"))
parse1 isComplete r =
  Text.Boomerang.Prim.parse1
    isComplete
    (Boomerang
      (runPrs r)
      (error "Text.Boomerang.EtCetera.Prim.parse1: Boomerang.Prim.parse1 evaluated serializer"))
