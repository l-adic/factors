{-# LANGUAGE TemplateHaskell #-}

module Main where

import Circom.CLI (defaultMain)
import Protolude
import ZK.Factors (Fr, factors)

main :: IO ()
main = defaultMain "circuit" $ factors @Fr
