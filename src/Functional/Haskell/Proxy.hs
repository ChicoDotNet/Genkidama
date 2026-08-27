import Data.IORef
import qualified Data.Map.Strict as Map

data Proxy = Proxy
  { backendCreated :: IORef Bool
  , fetchCount :: IORef Int
  , cache :: IORef (Map.Map Int String)
  }

newProxy :: IO Proxy
newProxy = Proxy <$> newIORef False <*> newIORef 0 <*> newIORef Map.empty

remoteGet :: Proxy -> Int -> IO String
remoteGet proxy ident = do
  writeIORef (backendCreated proxy) True
  modifyIORef' (fetchCount proxy) (+ 1)
  pure ("doc(" ++ show ident ++ ")")

proxyGet :: Proxy -> Int -> IO String
proxyGet proxy ident = do
  current <- readIORef (cache proxy)
  case Map.lookup ident current of
    Just value -> pure value
    Nothing -> do
      value <- remoteGet proxy ident
      modifyIORef' (cache proxy) (Map.insert ident value)
      pure value

main :: IO ()
main = do
  proxy <- newProxy
  first <- proxyGet proxy 42
  second <- proxyGet proxy 42
  backend <- readIORef (backendCreated proxy)
  fetches <- readIORef (fetchCount proxy)
  putStrLn $ "backend=" ++ (if backend then "1" else "0") ++ ";fetches=" ++ show fetches ++ ";first=" ++ first ++ ";second=" ++ second
