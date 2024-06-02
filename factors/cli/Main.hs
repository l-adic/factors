{-# LANGUAGE TemplateHaskell #-}

module Main where

import Circom.CLI (defaultMain)
import Circuit (BN128)
import Protolude
import ZK.Factors (factors)

main :: IO ()
main = defaultMain "circuit" $ factors @BN128
