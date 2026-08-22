module Main where

import Data.List (intercalate)

data ServiceProfile = ServiceProfile
  { profileName :: String,
    profileFeatures :: [String]
  }

cloneProfile :: ServiceProfile -> ServiceProfile
cloneProfile profile = profile {profileFeatures = map id (profileFeatures profile)}

describe :: ServiceProfile -> String
describe profile = profileName profile ++ ": " ++ intercalate "," (profileFeatures profile)

main :: IO ()
main = do
  let original = ServiceProfile "orders" ["metrics"]
      baseClone = cloneProfile original
      canary = baseClone {profileName = "orders-canary", profileFeatures = profileFeatures baseClone ++ ["tracing"]}
  putStrLn $ "original=" ++ describe original
  putStrLn $ "clone=" ++ describe canary
