module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

type Stuff =
  { foo :: forall a. a -> a
  , bar :: forall a. Eq a => a -> a
  }

v1 :: Stuff -> Int
v1 r = r.bar $ r.foo 1

main :: Effect Unit
main = log "Done"
