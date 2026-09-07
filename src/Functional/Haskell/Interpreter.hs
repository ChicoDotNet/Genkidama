module Main where

-- Interpreter in idiomatic Haskell: the grammar is an ADT and evaluation is
-- structural recursion over that grammar.
data Expr
  = Number Int
  | Add Expr Expr
  deriving (Eq, Show)

interpret :: Expr -> Int
interpret (Number value) = value
interpret (Add left right) = interpret left + interpret right

main :: IO ()
main = do
  let expr = Add (Number 2) (Add (Number 3) (Number 4))
      value = interpret expr
  if value /= 9
    then error ("expected 9, got " ++ show value)
    else putStrLn ("value=" ++ show value)
