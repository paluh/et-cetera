{-# LANGUAGE OverloadedStrings #-}

import qualified System.EtCetera.CollectdSpec as CollectdSpec
import qualified System.EtCetera.InterfacesSpec as InterfacesSpec
import qualified System.EtCetera.LxcSpec as LxcSpec
import           Test.Hspec (hspec)

main :: IO ()
main = hspec LxcSpec.suite -- (do InterfacesSpec.suite; CollectdSpec.suite; )
