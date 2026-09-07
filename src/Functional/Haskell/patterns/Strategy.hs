module Main where

type PricingStrategy = Int -> Int

price :: Int -> PricingStrategy -> Int
price value strategy = strategy value

regular :: PricingStrategy
regular value = value

vip :: PricingStrategy
vip value = value * 80 `div` 100

main :: IO ()
main =
  if price 100 regular == 100 && price 100 vip == 80
    then putStrLn "regular=100;vip=80"
    else error "Strategy contract failed"
