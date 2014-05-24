{-# LANGUAGE DeriveGeneric #-}
import Debug.Hoed.Observe

data T = Hello | World deriving Generic
instance Observable T

f :: T -> T
f Hello = World
f World = Hello

p :: T -> String
p Hello = "Hello"
p World = "world"


main = runO . putStrLn $ (p . f')  Hello
  where f' = gdmobserve "f" f
