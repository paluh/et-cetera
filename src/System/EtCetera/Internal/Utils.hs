module System.EtCetera.Internal.Utils where

import           Data.Char (toLower, toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c:cs
