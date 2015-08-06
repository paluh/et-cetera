{-# LANGUAGE OverloadedStrings #-}

import qualified System.EtCetera.InterfacesSpec as InterfacesSpec
import           Test.Hspec (hspec)

main :: IO ()
main = hspec InterfacesSpec.suite
