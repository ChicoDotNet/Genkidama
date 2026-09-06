module Main where

import qualified Data.Map.Strict as Map

type Colleague = String -> String -> String

data CheckoutMediator = CheckoutMediator (Map.Map String Colleague)

register :: String -> Colleague -> CheckoutMediator -> CheckoutMediator
register name receiver (CheckoutMediator colleagues) =
  CheckoutMediator (Map.insert name receiver colleagues)

send :: CheckoutMediator -> String -> String -> String -> Either String String
send (CheckoutMediator colleagues) sender recipient message =
  case Map.lookup recipient colleagues of
    Just receiver -> Right (receiver sender message)
    Nothing -> Left ("unknown colleague: " ++ recipient)

payment :: Colleague
payment sender message = "payment<-" ++ sender ++ ":" ++ message

inventory :: Colleague
inventory sender message = "inventory<-" ++ sender ++ ":" ++ message

verifyMediator :: Bool
verifyMediator =
  let mediator =
        register "inventory" inventory $
          register "payment" payment $
            CheckoutMediator Map.empty
   in send mediator "payment" "inventory" "paid"
        == Right "inventory<-payment:paid"
        && send mediator "inventory" "payment" "reserved"
          == Right "payment<-inventory:reserved"
        && send mediator "payment" "shipping" "dispatch"
          == Left "unknown colleague: shipping"

main :: IO ()
main =
  if verifyMediator
    then putStrLn "Haskell Mediator: passed"
    else error "Haskell Mediator failed"
