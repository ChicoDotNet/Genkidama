module Main where

data Command = Deposit Int | Withdraw Int deriving (Eq, Show)

execute :: Int -> Command -> Int
execute balance (Deposit amount) = balance + amount
execute balance (Withdraw amount) = balance - amount

main :: IO ()
main = do
  let queue = [Deposit 50, Withdraw 20]
      balance = foldl execute 100 queue
  if balance == 130 && length queue == 2
    then putStrLn "balance=130;commands=2"
    else error "Command contract failed"
