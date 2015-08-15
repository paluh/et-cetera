{-# LANGUAGE OverloadedStrings #-}

import qualified System.EtCetera.InterfacesSpec as InterfacesSpec
import qualified System.EtCetera.CollectdSpec as CollectdSpec
import           Test.Hspec (hspec)

main :: IO ()
main = hspec (do InterfacesSpec.suite; CollectdSpec.suite;)
