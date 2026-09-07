module Observer (Observer, Subscription, subscribe, unsubscribe, publish, examplePasses) where

type Observer a b = a -> b
type Subscription a b = (String, Observer a b)

subscribe :: String -> Observer a b -> [Subscription a b] -> [Subscription a b]
subscribe key observer subscriptions
  | any ((== key) . fst) subscriptions = subscriptions
  | otherwise = subscriptions ++ [(key, observer)]

unsubscribe :: String -> [Subscription a b] -> [Subscription a b]
unsubscribe key = filter ((/= key) . fst)

publish :: [Subscription a b] -> a -> [b]
publish subscriptions value = map (($ value) . snd) subscriptions

examplePasses :: Bool
examplePasses =
  let audit value = "audit:" ++ show value
      dashboard value = "dashboard:" ++ show value
      initial = subscribe "audit" audit []
      both = subscribe "dashboard" dashboard initial
      duplicateRejected = subscribe "audit" audit both
      afterUnsubscribe = unsubscribe "audit" duplicateRejected
   in publish duplicateRejected (42 :: Int) == ["audit:42", "dashboard:42"]
        && length duplicateRejected == 2
        && publish afterUnsubscribe 43 == ["dashboard:43"]
