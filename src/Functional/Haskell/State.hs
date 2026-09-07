module Main where

data GateState = Locked | Unlocked
  deriving (Eq, Show)

data GateAction = InsertCoin | Push
  deriving (Eq, Show)

data GateResult = CoinAccepted | CoinReturned | Passed | Blocked
  deriving (Eq, Show)

transition :: GateState -> GateAction -> (GateState, GateResult)
transition Locked InsertCoin = (Unlocked, CoinAccepted)
transition Locked Push = (Locked, Blocked)
transition Unlocked InsertCoin = (Unlocked, CoinReturned)
transition Unlocked Push = (Locked, Passed)

must :: Bool -> IO ()
must True = pure ()
must False = error "State contract failed"

main :: IO ()
main = do
  let initial = Locked
      (afterBlockedPush, blockedResult) = transition initial Push
      (afterCoin, coinResult) = transition afterBlockedPush InsertCoin
      (afterDuplicateCoin, duplicateCoinResult) = transition afterCoin InsertCoin
      (afterPass, passResult) = transition afterDuplicateCoin Push

  must (initial == Locked)
  must (afterBlockedPush == Locked && blockedResult == Blocked)
  must (afterCoin == Unlocked && coinResult == CoinAccepted)
  must (afterDuplicateCoin == Unlocked && duplicateCoinResult == CoinReturned)
  must (afterPass == Locked && passResult == Passed)

  putStrLn "haskell-state: passed"
