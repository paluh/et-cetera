{-# LANGUAGE OverloadedStrings #-}

import qualified System.EtCetera.CollectdSpec as CollectdSpec
import qualified System.EtCetera.InternalSpec as InternalSpec
import qualified System.EtCetera.InterfacesSpec as InterfacesSpec
import qualified System.EtCetera.LxcSpec as LxcSpec
import qualified System.EtCetera.RedisSpec as RedisSpec
import           Test.Hspec (hspec)

-- main = hspec RedisSpec.suite
-- (do InterfacesSpec.suite; CollectdSpec.suite; )
main :: IO ()
main = hspec $ do
  InternalSpec.suite
  RedisSpec.suite
  LxcSpec.suite
  InterfacesSpec.suite
  CollectdSpec.suite
