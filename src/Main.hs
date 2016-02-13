module Main where

import           Control.Category (id, (.))
import           Data.Monoid ((<>))
import           Prelude hiding (id, (.))
import           Test.Hspec (describe, it, shouldBe, Spec)
import           Text.Boomerang.Combinators (manyl, opt, push)
import           Text.Boomerang.String (lit)
import           System.EtCetera.Redis.V2_8 (emptyConfig, parse, serialize)
import           System.EtCetera.Internal.Prim (toPrs, Prs(..))
import           System.EtCetera.Internal.Boomerangs (parseString)


-- parser =
--   bPrs (manyl (lit " "))
--   . (id <> (eol . parser))
--  where
--   eol = bPrs (lit "\n")
-- 
-- -- parse :: String -> Either ParsingError RedisConfig
-- parse =
--   parseString (parser . bPrs (push emptyConfig))

main :: IO ()
main = do
  print $ emptyConfig
  print $ parse "   "
  print $ parse "include /mnt/rootfs.complex"
