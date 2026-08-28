module Main where

import Data.List (intercalate)

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Int
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

data Shape = Circle Int | Rectangle Int Int
  deriving (Eq, Show)

area :: Shape -> Int
area (Circle r) = 3 * r * r
area (Rectangle w h) = w * h

perimeter :: Shape -> Int
perimeter (Circle r) = 6 * r
perimeter (Rectangle w h) = 2 * (w + h)

data UiState = LoggedOut | LoggedIn
  deriving (Eq, Show)

data Document = Document { docText :: String }
  deriving (Eq, Show)

data Record = Record { recordId :: Int, recordName :: String }
  deriving (Eq, Show)

type Store = [(Int, String)]

type Test = (String, Bool)

commandExample :: Bool
commandExample =
  let commands = [("deposit", 10), ("withdraw", -3)]
      executed = scanl (+) 0 (map snd commands)
      undone = last executed - snd (last commands)
  in executed == [0, 10, 7] && undone == 10

interpreterExample :: Bool
interpreterExample = eval (Add (Lit 2) (Mul (Lit 3) (Lit 4))) == 14

iteratorExample :: Bool
iteratorExample =
  let next xs cursor = if cursor < length xs then Just (xs !! cursor, cursor + 1) else Nothing
  in next [10,20] 0 == Just (10,1) && next [10,20] 2 == Nothing

mediatorExample :: Bool
mediatorExample =
  let mediate sender msg = if sender == "sales" then ("billing", msg) else ("sales", msg)
  in mediate "sales" "invoice" == ("billing", "invoice")

mementoExample :: Bool
mementoExample =
  let current = Document "v2"
      snapshot = Document "v1"
      restored = snapshot
  in docText current == "v2" && restored == Document "v1"

observerExample :: Bool
observerExample =
  let publish value observers = map ($ value) observers
      observers = [(\x -> "audit:" ++ show x), (\x -> "ui:" ++ show x)]
  in publish (7 :: Int) observers == ["audit:7", "ui:7"]

stateExample :: Bool
stateExample =
  let action LoggedOut = (LoggedIn, "login")
      action LoggedIn = (LoggedOut, "logout")
  in action LoggedOut == (LoggedIn, "login") && action LoggedIn == (LoggedOut, "logout")

strategyExample :: Bool
strategyExample =
  let price strategy amount = strategy amount
      regular x = x
      discounted x = x * 80 `div` 100
  in price regular 100 == 100 && price discounted 100 == 80

templateMethodExample :: Bool
templateMethodExample =
  let run transform input = ["open", transform input, "close"]
  in run reverse "abc" == ["open", "cba", "close"]

visitorExample :: Bool
visitorExample =
  let shapes = [Circle 2, Rectangle 3 4]
  in map area shapes == [12,12] && map perimeter shapes == [12,14]

mvcExample :: Bool
mvcExample =
  let model = 3
      controller m = m + 1
      view m = "count=" ++ show m
  in view (controller model) == "count=4"

mvvmExample :: Bool
mvvmExample =
  let model = ("Ada", True)
      viewModel (name, enabled) = ("Hello " ++ name, if enabled then "enabled" else "disabled")
  in viewModel model == ("Hello Ada", "enabled")

microkernelExample :: Bool
microkernelExample =
  let core plugins name value = case lookup name plugins of
        Just plugin -> plugin value
        Nothing -> value
      plugins = [("double", (*2)), ("square", (\x -> x*x))]
  in core plugins "double" (5 :: Int) == 10

microservicesExample :: Bool
microservicesExample =
  let inventory sku = if sku == "A" then 3 else 0
      pricing sku = if sku == "A" then 20 else 0
      gateway sku = (inventory sku, pricing sku)
  in gateway "A" == (3,20)

enterpriseAdapterExample :: Bool
enterpriseAdapterExample =
  let legacy cents = cents
      adapt dollars = legacy (dollars * 100)
  in adapt 12 == 1200

enterpriseBridgeExample :: Bool
enterpriseBridgeExample =
  let render transport payload = transport payload
      http p = "http:" ++ p
      queue p = "queue:" ++ p
  in render http "x" == "http:x" && render queue "x" == "queue:x"

enterpriseFacadeExample :: Bool
enterpriseFacadeExample =
  let validate x = x > 0
      persist x = "saved:" ++ show x
      facade x = if validate x then persist x else "rejected"
  in facade (5 :: Int) == "saved:5"

brokerExample :: Bool
brokerExample =
  let registry = [("tax", (\x -> x * 16 `div` 100))]
      call name x = maybe 0 ($ x) (lookup name registry)
  in call "tax" (100 :: Int) == 16

messageBusExample :: Bool
messageBusExample =
  let subscribers = [("audit", (\m -> "audit:" ++ m)), ("mail", (\m -> "mail:" ++ m))]
      publish msg = map (\(_, handler) -> handler msg) subscribers
  in publish "paid" == ["audit:paid", "mail:paid"]

serviceLocatorExample :: Bool
serviceLocatorExample =
  let services = [("clock", "12:00"), ("region", "mx")]
  in lookup "region" services == Just "mx"

activeObjectExample :: Bool
activeObjectExample =
  let enqueue queue command = queue ++ [command]
      schedule [] = ([], [])
      schedule (x:xs) = (["run:" ++ x], xs)
      queued = enqueue [] "sync"
  in schedule queued == (["run:sync"], [])

monitorObjectExample :: Bool
monitorObjectExample =
  let deposit amount balance = balance + amount
      withdraw amount balance = if balance >= amount then balance - amount else balance
      afterDeposit = deposit 10 5
  in withdraw 7 afterDeposit == 8

halfSyncHalfAsyncExample :: Bool
halfSyncHalfAsyncExample =
  let asyncArrive queue event = queue ++ [event]
      syncProcess [] = Nothing
      syncProcess (x:xs) = Just ("processed:" ++ x, xs)
      queue = asyncArrive [] "evt"
  in syncProcess queue == Just ("processed:evt", [])

leaderFollowersExample :: Bool
leaderFollowersExample =
  let dispatch (leader:followers) event = (leader ++ ":" ++ event, followers ++ [leader])
      dispatch [] _ = ("none", [])
  in dispatch ["a","b","c"] "evt" == ("a:evt", ["b","c","a"])

clientServerExample :: Bool
clientServerExample =
  let server request = "response(" ++ request ++ ")"
      client request = server request
  in client "ping" == "response(ping)"

peerToPeerExample :: Bool
peerToPeerExample =
  let send from to payload = from ++ "->" ++ to ++ ":" ++ payload
  in send "a" "b" "x" == "a->b:x" && send "b" "a" "y" == "b->a:y"

publishSubscribeExample :: Bool
publishSubscribeExample =
  let subscriptions = [("orders", ["audit", "warehouse"]), ("users", ["crm"])]
      publish topic = maybe [] id (lookup topic subscriptions)
  in publish "orders" == ["audit", "warehouse"]

distributedProxyExample :: Bool
distributedProxyExample =
  let remote idValue = "remote-user-" ++ show idValue
      proxy idValue = remote idValue
  in proxy (7 :: Int) == "remote-user-7"

presentationAbstractionControlExample :: Bool
presentationAbstractionControlExample =
  let abstraction = 4
      control model action = if action == "inc" then model + 1 else model
      presentation model = "value=" ++ show model
  in presentation (control abstraction "inc") == "value=5"

modelViewPresenterExample :: Bool
modelViewPresenterExample =
  let model = "Ada"
      presenter value = "Hello " ++ value
      passiveView text = "[" ++ text ++ "]"
  in passiveView (presenter model) == "[Hello Ada]"

documentViewExample :: Bool
documentViewExample =
  let document = Document "hello"
      plainView d = docText d
      upperView d = map toUpperAscii (docText d)
      toUpperAscii c | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
                     | otherwise = c
  in plainView document == "hello" && upperView document == "HELLO"

activeRecordExample :: Bool
activeRecordExample =
  let save (Record rid name) store = (rid, name) : filter ((/= rid) . fst) store
      row = Record 1 "Ada"
  in save row [] == [(1,"Ada")]

dataMapperExample :: Bool
dataMapperExample =
  let toRow (Record rid name) = (rid, name)
      fromRow (rid, name) = Record rid name
  in fromRow (toRow (Record 1 "Ada")) == Record 1 "Ada"

unitOfWorkExample :: Bool
unitOfWorkExample =
  let stage changes change = changes ++ [change]
      commit store changes = store ++ changes
      pending = stage [] (1,"Ada")
  in commit [] pending == [(1,"Ada")]

repositoryExample :: Bool
repositoryExample =
  let findById rid store = lookup rid store
      save rid name store = (rid,name) : filter ((/= rid) . fst) store
      store = save 1 "Ada" []
  in findById 1 store == Just "Ada"

dependencyInjectionExample :: Bool
dependencyInjectionExample =
  let service clock = "time=" ++ clock
  in service "12:00" == "time=12:00"

lazyInitializationExample :: Bool
lazyInitializationExample =
  let getOrCreate Nothing factory = (factory (), True)
      getOrCreate (Just x) _ = (x, False)
      factory () = "resource"
  in getOrCreate Nothing factory == ("resource", True) && getOrCreate (Just "resource") factory == ("resource", False)

objectPoolExample :: Bool
objectPoolExample =
  let acquire (x:xs) = Just (x, xs)
      acquire [] = Nothing
      release x xs = xs ++ [x]
  in case acquire ["c1","c2"] of
       Just (resource, rest) -> release resource rest == ["c2","c1"]
       Nothing -> False

nullObjectExample :: Bool
nullObjectExample =
  let run logger msg = logger msg
      realLogger msg = "log:" ++ msg
      nullLogger _ = ""
  in run realLogger "x" == "log:x" && run nullLogger "x" == ""

tests :: [Test]
tests =
  [ ("Command", commandExample)
  , ("Interpreter", interpreterExample)
  , ("Iterator", iteratorExample)
  , ("Mediator", mediatorExample)
  , ("Memento", mementoExample)
  , ("Observer", observerExample)
  , ("State", stateExample)
  , ("Strategy", strategyExample)
  , ("Template Method", templateMethodExample)
  , ("Visitor", visitorExample)
  , ("MVC", mvcExample)
  , ("MVVM", mvvmExample)
  , ("Microkernel", microkernelExample)
  , ("Microservices", microservicesExample)
  , ("Enterprise Adapter", enterpriseAdapterExample)
  , ("Enterprise Bridge", enterpriseBridgeExample)
  , ("Enterprise Facade", enterpriseFacadeExample)
  , ("Broker", brokerExample)
  , ("Message Bus", messageBusExample)
  , ("Service Locator", serviceLocatorExample)
  , ("Active Object", activeObjectExample)
  , ("Monitor Object", monitorObjectExample)
  , ("Half-Sync / Half-Async", halfSyncHalfAsyncExample)
  , ("Leader / Followers", leaderFollowersExample)
  , ("Client-Server", clientServerExample)
  , ("Peer-to-Peer", peerToPeerExample)
  , ("Publish-Subscribe", publishSubscribeExample)
  , ("Distributed Proxy", distributedProxyExample)
  , ("Presentation-Abstraction-Control", presentationAbstractionControlExample)
  , ("Model-View-Presenter", modelViewPresenterExample)
  , ("Document-View", documentViewExample)
  , ("Active Record", activeRecordExample)
  , ("Data Mapper", dataMapperExample)
  , ("Unit of Work", unitOfWorkExample)
  , ("Repository", repositoryExample)
  , ("Dependency Injection", dependencyInjectionExample)
  , ("Lazy Initialization", lazyInitializationExample)
  , ("Object Pool", objectPoolExample)
  , ("Null Object", nullObjectExample)
  ]

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in if null failed
       then putStrLn ("Haskell pattern sweep: " ++ show (length tests) ++ "/" ++ show (length tests) ++ " examples passed")
       else error ("Haskell pattern sweep failures: " ++ intercalate ", " failed)
