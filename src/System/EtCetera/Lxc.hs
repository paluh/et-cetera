-- I had problem with native lxc library
-- config handling so here is pure
-- haskell replacement
module System.EtCetera.Lxc
  ( emptyConfig
  , emptyNetwork
  , LxcConfig(..)
  , Network(..)
  , NetworkType(..)
  , parse
  , ParsingError
  , serialize
  , SerializtionError
  , Switch(..))
  where

import System.EtCetera.Lxc.Internal
