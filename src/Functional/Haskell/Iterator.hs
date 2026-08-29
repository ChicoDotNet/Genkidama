module Main where

data Iterator a = Iterator [a]

next :: Iterator a -> (Maybe a, Iterator a)
next (Iterator []) = (Nothing, Iterator [])
next (Iterator (x : xs)) = (Just x, Iterator xs)

main :: IO ()
main = do
  let (a, i1) = next (Iterator [10, 20, 30 :: Int])
      (b, i2) = next i1
      (c, i3) = next i2
      (d, _) = next i3
  if [a, b, c] == map Just [10, 20, 30] && d == Nothing
    then putStrLn "iterator=10,20,30"
    else error "iterator contract failed"
