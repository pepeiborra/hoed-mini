{-# LANGUAGE DeriveGeneric #-}
import Debug.Hoed.Observe

data Basket = FruitBasket { appels :: Int, pears :: Int } deriving (Generic)
instance Observable Basket

s FruitBasket {appels=a,pears=p} = a + p
b = FruitBasket {appels=40,pears=2}

main = (runO . putStrLn . show) (s $ gdmobserve "b" b)
