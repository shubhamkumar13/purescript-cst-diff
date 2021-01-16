module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log person.name

type Person = Record (name :: String, age :: Int)

person :: Person 
person = { name: "Mike", age: 1 }