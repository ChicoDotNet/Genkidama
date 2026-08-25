module Main where

authenticate :: String -> String
authenticate user = "auth(" ++ user ++ ")"

reserve :: String -> String
reserve sku = "reserve(" ++ sku ++ ")"

charge :: Int -> String
charge cents = "charge(" ++ show cents ++ ")"

checkout :: String -> String -> Int -> String
checkout user sku cents =
    "checkout=" ++ authenticate user ++ ">" ++ reserve sku ++ ">" ++ charge cents

main :: IO ()
main = putStrLn (checkout "alice" "SKU-42" 499)
