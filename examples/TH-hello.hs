{-# LANGUAGE TemplateHaskell, Rank2Types #-}
import Debug.Hoed.Observe

data T = Hello | World

f :: T -> T
f Hello = World
f World = Hello

p :: T -> String
p Hello = "Hello"
p World = "world"

$(observedTypes "f" [[t| T |]])
main = runO . putStrLn $ (p . f') Hello
  where f' = $(observe "f") f
