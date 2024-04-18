module Prove (fullProve) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Control.Promise (Promise, toAffE)


foreign import fullProveImpl :: {a::Int, b::Int, n :: Int} -> Effect (Promise Unit)

fullProve :: {a :: Int, b :: Int, n :: Int} -> Aff Unit
fullProve input = toAffE $ fullProveImpl input