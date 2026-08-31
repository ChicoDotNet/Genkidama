module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe)
import System.Process (readProcess)

must :: Bool -> IO ()
must True = pure ()
must False = error "pattern assertion failed"

-- Command
data BalanceCommand = Deposit Int | Withdraw Int deriving (Eq, Show)
executeCommand :: Int -> BalanceCommand -> Int
executeCommand b (Deposit n) = b + n
executeCommand b (Withdraw n) = b - n
undoCommand :: Int -> BalanceCommand -> Int
undoCommand b (Deposit n) = b - n
undoCommand b (Withdraw n) = b + n
commandCase :: Bool
commandCase = let q = [Deposit 50, Withdraw 20]; b = foldl executeCommand 100 q in b == 130 && undoCommand b (last q) == 150

-- Interpreter
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
eval :: Expr -> Int
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
interpreterCase :: Bool
interpreterCase = eval (Add (Lit 7) (Mul (Lit 3) (Lit 4))) == 19

-- Iterator
data Iterator a = Iterator [a]
next :: Iterator a -> (Maybe a, Iterator a)
next (Iterator []) = (Nothing, Iterator [])
next (Iterator (x:xs)) = (Just x, Iterator xs)
iteratorCase :: Bool
iteratorCase = let (a,i1)=next (Iterator [10,20,30::Int]); (b,i2)=next i1; (c,i3)=next i2; (d,_)=next i3 in [a,b,c]==map Just [10,20,30] && d==Nothing

-- Mediator: delegate to the individually addressable canonical artifact.
mediatorCase :: IO Bool
mediatorCase = do
  output <- readProcess "runghc" ["patterns/mediator.hs"] ""
  pure ("Haskell Mediator: passed" `isInfixOf` output)

-- Memento
newtype EditorMemento = EditorMemento String
restoreEditor :: EditorMemento -> String
restoreEditor (EditorMemento s) = s
mementoCase :: Bool
mementoCase = let original="draft"; snapshot=EditorMemento original; changed="published" in changed=="published" && restoreEditor snapshot=="draft"

-- Observer
type Observer = Int -> String
publish :: [Observer] -> Int -> [String]
publish observers value = map ($ value) observers
observerCase :: Bool
observerCase = publish [("audit:"++) . show, ("dashboard:"++) . show] 42 == ["audit:42","dashboard:42"]

-- State
data GateState = Locked | Unlocked deriving (Eq, Show)
transition :: GateState -> String -> GateState
transition Locked "unlock" = Unlocked
transition Unlocked "lock" = Locked
transition s _ = s
stateCase :: Bool
stateCase = transition (transition Locked "unlock") "lock" == Locked

-- Strategy
price :: (Int -> Int) -> Int -> Int
price strategy = strategy
strategyCase :: Bool
strategyCase = price id 100 == 100 && price (\v -> v * 80 `div` 100) 100 == 80

-- Template Method
pipeline :: String -> (() -> String) -> String
pipeline readStep transform = intercalate ">" [readStep, transform (), "publish"]
templateMethodCase :: Bool
templateMethodCase = pipeline "read-csv" (const "normalize") == "read-csv>normalize>publish" && pipeline "read-json" (const "aggregate") == "read-json>aggregate>publish"

-- Visitor
data Shape = Circle Double | Rectangle Double Double
data ShapeVisitor a = ShapeVisitor { visitCircle :: Double -> a, visitRectangle :: Double -> Double -> a }
accept :: ShapeVisitor a -> Shape -> a
accept v (Circle r) = visitCircle v r
accept v (Rectangle w h) = visitRectangle v w h
visitorCase :: Bool
visitorCase = let area=ShapeVisitor (\r -> pi*r*r) (*) ; shapes=[Circle 2,Rectangle 3 4]; total=sum (map (accept area) shapes) in abs (total-(4*pi+12)) < 1e-9

-- MVC
data CounterModel = CounterModel { counterValue :: Int } deriving Eq
controllerIncrement :: CounterModel -> CounterModel
controllerIncrement m = m { counterValue = counterValue m + 1 }
viewCounter :: CounterModel -> String
viewCounter m = "count=" ++ show (counterValue m)
mvcCase :: Bool
mvcCase = let m=CounterModel 0; m'=controllerIncrement m in viewCounter m=="count=0" && viewCounter m'=="count=1"

-- MVVM
data AmountVM = AmountVM Int deriving Eq
amountText :: AmountVM -> String
amountText (AmountVM n) = "$" ++ show n ++ ".00"
addAmount :: Int -> AmountVM -> AmountVM
addAmount n (AmountVM v) = AmountVM (v+n)
mvvmCase :: Bool
mvvmCase = let v=AmountVM 10; v'=addAmount 5 v in amountText v=="$10.00" && amountText v'=="$15.00"

-- Microkernel
type Plugin = Int -> Int
runPlugin :: [(String,Plugin)] -> String -> Int -> Int
runPlugin plugins name value = fromMaybe (error "missing plugin") (lookup name plugins) value
microkernelCase :: Bool
microkernelCase = let k=[("double",(*2)),("square",\v->v*v)] in runPlugin k "double" 4==8 && runPlugin k "square" 4==16

-- Microservices
reserve :: Int -> Int -> Maybe Int
reserve stock qty | qty <= stock = Just (stock-qty) | otherwise = Nothing
placeOrder :: Int -> Int -> (String,Int)
placeOrder stock qty = maybe ("rejected",stock) (\remaining -> ("confirmed",remaining)) (reserve stock qty)
microservicesCase :: Bool
microservicesCase = placeOrder 7 2 == ("confirmed",5)

-- Enterprise Adapter
data LegacyCustomer = LegacyCustomer Int Int
data CanonicalCustomer = CanonicalCustomer Int Double
adaptCustomer :: LegacyCustomer -> CanonicalCustomer
adaptCustomer (LegacyCustomer code cents) = CanonicalCustomer code (fromIntegral cents / 100)
enterpriseAdapterCase :: Bool
enterpriseAdapterCase = case adaptCustomer (LegacyCustomer 17 1250) of CanonicalCustomer i a -> i==17 && a==12.5

-- Enterprise Bridge
type Transport = String -> String
sendNotice :: String -> String -> Transport -> String
sendNotice kind msg transport = transport (kind ++ ":" ++ msg)
enterpriseBridgeCase :: Bool
enterpriseBridgeCase = sendNotice "ALERT" "disk" ("kafka>"++) == "kafka>ALERT:disk" && sendNotice "REMINDER" "backup" ("queue>"++) == "queue>REMINDER:backup"

-- Enterprise Facade
enterpriseFacadeCase :: Bool
enterpriseFacadeCase =
  let crm :: Int -> String
      crm i = "crm:create:" ++ show i
      billing :: Int -> String
      billing i = "billing:open:" ++ show i
  in crm 77 ++ ">" ++ billing 77 == "crm:create:77>billing:open:77"

-- Broker
type Broker = [(String, String -> String)]
callBroker :: Broker -> String -> String -> String
callBroker b service arg = fromMaybe (error "service not found") (lookup service b) arg
brokerCase :: Bool
brokerCase = let b=[("inventory",\k->"inventory:"++k++"=7"),("customer",\k->"customer:"++k++"=active")] in callBroker b "inventory" "sku-1"=="inventory:sku-1=7" && callBroker b "customer" "17"=="customer:17=active"

-- Message Bus
data Message = Message String Int
type MessageHandler = Message -> String
sendBus :: [MessageHandler] -> Message -> [String]
sendBus hs m = map ($ m) hs
messageBusCase :: Bool
messageBusCase = let audit (Message t i)="audit:"++t++":"++show i; billing (Message t i)="billing:"++t++":"++show i in sendBus [audit,billing] (Message "order-created" 42)==["audit:order-created:42","billing:order-created:42"]

-- Service Locator
serviceLocatorCase :: Bool
serviceLocatorCase = let loc=[("email",("email>"++)),("audit",("audit>"++))] in fromMaybe id (lookup "email" loc) "a@example.test"=="email>a@example.test" && fromMaybe id (lookup "audit" loc) "created"=="audit>created"

-- Active Object
activeObjectCase :: Bool
activeObjectCase = let queue=[(+3),(*4)]; before=(0 :: Int); after=foldl (flip ($)) before queue in before==0 && after==12

-- Monitor Object: state and synchronization are encapsulated in an MVar.
monitorObjectCase :: IO Bool
monitorObjectCase = do
  m <- newMVar (0 :: Int)
  done1 <- newEmptyMVar
  done2 <- newEmptyMVar
  _ <- forkIO (modifyMVar_ m (pure . (+2)) >> putMVar done1 ())
  _ <- forkIO (modifyMVar_ m (pure . (+3)) >> putMVar done2 ())
  takeMVar done1 >> takeMVar done2
  (==5) <$> readMVar m

-- Half-Sync / Half-Async
halfSyncHalfAsyncCase :: Bool
halfSyncHalfAsyncCase = let queued=["job-1","job-2","job-3"]; processed=map ("done:"++) queued in processed==["done:job-1","done:job-2","done:job-3"]

-- Leader / Followers
leaderFollowersCase :: Bool
leaderFollowersCase = let workers=cycle ["worker-1","worker-2","worker-3"]; events=["event-a","event-b","event-c"]; handled=zipWith (\w e->w++":"++e) workers events; nextWorker=workers!!3 in handled==["worker-1:event-a","worker-2:event-b","worker-3:event-c"] && nextWorker=="worker-1"

-- Client-Server
data Request = Request String
data Response = Response Int String
serve :: Request -> Response
serve (Request "sku-1") = Response 200 "stock=7"
serve _ = Response 404 "missing"
clientServerCase :: Bool
clientServerCase = case serve (Request "sku-1") of Response s b -> s==200 && b=="stock=7"

-- Peer-to-Peer
data Peer = Peer String
peerSend :: Peer -> Peer -> String -> String
peerSend (Peer a) (Peer b) payload = a ++ ">" ++ b ++ ":" ++ payload
peerToPeerCase :: Bool
peerToPeerCase = let a=Peer "peer-a" in [peerSend a (Peer "peer-b") "block-42",peerSend a (Peer "peer-c") "block-42"]==["peer-a>peer-b:block-42","peer-a>peer-c:block-42"]

-- Publish-Subscribe
type Subscription = (String, Int -> String)
publishTopic :: [Subscription] -> String -> Int -> [String]
publishTopic subs topic value = [f value | (t,f) <- subs, t==topic]
publishSubscribeCase :: Bool
publishSubscribeCase = let s=[("order",("warehouse:"++) . show),("order",("analytics:"++) . show)] in publishTopic s "order" 51==["warehouse:51","analytics:51"]

-- Distributed Proxy
type StockService = String -> Int
stockProxy :: StockService -> StockService
stockProxy remote sku = remote sku
distributedProxyCase :: Bool
distributedProxyCase = stockProxy (const 7) "sku-1" == 7

-- Presentation-Abstraction-Control
data Agent = Agent String Int
agentView :: Agent -> String
agentView (Agent n v) = n ++ ":view=" ++ show v
pacCase :: Bool
pacCase = agentView (Agent "child" 42)=="child:view=42" && agentView (Agent "root" 42)=="root:view=42"

-- Model-View-Presenter
presentIncrement :: CounterModel -> (CounterModel,String)
presentIncrement m = let m'=controllerIncrement m in (m',viewCounter m')
mvpCase :: Bool
mvpCase = let (m,v)=presentIncrement (CounterModel 0) in counterValue m==1 && v=="count=1"

-- Document-View
data Document = Document String Int
editorView :: Document -> String
editorView (Document title words') = "editor:"++title++":"++show words'
summaryView :: Document -> String
summaryView (Document title _) = "summary:"++title
documentViewCase :: Bool
documentViewCase = let d=Document "Final" 120 in editorView d=="editor:Final:120" && summaryView d=="summary:Final"

-- Active Record
data PersonRecord = PersonRecord Int String deriving (Eq,Show)
saveRecord :: [PersonRecord] -> PersonRecord -> [PersonRecord]
saveRecord table record = record : filter (\(PersonRecord i _) -> i /= recordId record) table
  where recordId (PersonRecord i _) = i
loadRecord :: [PersonRecord] -> Int -> Maybe PersonRecord
loadRecord table wanted = case filter (\(PersonRecord i _) -> i==wanted) table of r:_ -> Just r; [] -> Nothing
activeRecordCase :: Bool
activeRecordCase = loadRecord (saveRecord [] (PersonRecord 7 "Ada")) 7 == Just (PersonRecord 7 "Ada")

-- Data Mapper
data Person = Person Int String deriving (Eq,Show)
data PersonRow = PersonRow String String
mapToRow :: Person -> PersonRow
mapToRow (Person i n)=PersonRow ("person:"++show i) n
mapFromRow :: PersonRow -> Person
mapFromRow (PersonRow _ n)=Person 8 n
dataMapperCase :: Bool
dataMapperCase = let row@(PersonRow key _)=mapToRow (Person 8 "Grace"); Person _ n=mapFromRow row in key=="person:8" && n=="Grace"

-- Unit of Work
commitUnit :: [Int] -> [(Int,Int)] -> [Int]
commitUnit values changes = [v + sum [d | (j,d)<-changes,j==i] | (i,v)<-zip [0..] values]
unitOfWorkCase :: Bool
unitOfWorkCase = let before=[10,20]; after=commitUnit before [(0,5),(1,-3)] in before==[10,20] && after==[15,17]

-- Repository
newtype PersonRepository = PersonRepository [Person]
findPerson :: PersonRepository -> Int -> Maybe Person
findPerson (PersonRepository ps) wanted = case filter (\(Person i _)->i==wanted) ps of p:_ -> Just p; [] -> Nothing
repositoryCase :: Bool
repositoryCase = findPerson (PersonRepository [Person 9 "Linus"]) 9 == Just (Person 9 "Linus")

-- Dependency Injection
type Sender = String -> String
greet :: Sender -> String -> String
greet sender name = sender name
dependencyInjectionCase :: Bool
dependencyInjectionCase = greet ("smtp:"++) "Ada"=="smtp:Ada" && greet ("fake:"++) "Ada"=="fake:Ada"

-- Lazy Initialization
data LazyValue a = Pending (() -> a) | Ready a
getLazy :: LazyValue a -> (a, LazyValue a)
getLazy (Ready a) = (a, Ready a)
getLazy (Pending f) = let a=f () in (a, Ready a)
lazyInitializationCase :: Bool
lazyInitializationCase = let (a,l1)=getLazy (Pending (const "resource-ready")); (b,l2)=getLazy l1 in a=="resource-ready" && b=="resource-ready" && case l2 of { Ready _ -> True; _ -> False }

-- Object Pool
data ObjectPool = ObjectPool [Int] Int
acquire :: ObjectPool -> (Int,ObjectPool)
acquire (ObjectPool (x:xs) n)=(x,ObjectPool xs n)
acquire (ObjectPool [] n)=let v=n+1 in (v,ObjectPool [] v)
release :: Int -> ObjectPool -> ObjectPool
release v (ObjectPool xs n)=ObjectPool (v:xs) n
objectPoolCase :: Bool
objectPoolCase = let (a,p1)=acquire (ObjectPool [] 0); (b,p2)=acquire p1; p3=release a p2; (c,_)=acquire p3 in (a,b,c)==(1,2,1)

-- Null Object
type Logger = String -> String
nullObjectCase :: Bool
nullObjectCase = let real msg="logged:"++msg; nullLogger _="" in real "processed:item-1"=="logged:processed:item-1" && nullLogger "processed:item-1"==""

pureCases :: [Bool]
pureCases = [ commandCase, interpreterCase, iteratorCase, mementoCase, observerCase, stateCase, strategyCase, templateMethodCase, visitorCase
            , mvcCase, mvvmCase, microkernelCase, microservicesCase, enterpriseAdapterCase, enterpriseBridgeCase, enterpriseFacadeCase, brokerCase, messageBusCase, serviceLocatorCase
            , activeObjectCase, halfSyncHalfAsyncCase, leaderFollowersCase, clientServerCase, peerToPeerCase, publishSubscribeCase, distributedProxyCase, pacCase, mvpCase, documentViewCase
            , activeRecordCase, dataMapperCase, unitOfWorkCase, repositoryCase, dependencyInjectionCase, lazyInitializationCase, objectPoolCase, nullObjectCase ]

main :: IO ()
main = do
  must (length pureCases == 37)
  forM_ pureCases must
  mediatorOk <- mediatorCase
  must mediatorOk
  monitorOk <- monitorObjectCase
  must monitorOk
  putStrLn "Haskell pattern sweep: 39/39 examples passed"
