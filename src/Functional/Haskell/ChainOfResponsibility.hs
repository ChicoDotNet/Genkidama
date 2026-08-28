data Handler = Handler { name :: String, accepts :: Int -> Bool }

handlers :: [Handler]
handlers =
  [ Handler "faq" (<= 50)
  , Handler "billing" (<= 500)
  , Handler "escalation" (const True)
  ]

handle :: Int -> [Handler] -> [String] -> ([String], String, String)
handle _ [] _ = error "unhandled request"
handle amount (h:rest) visited =
  let visited' = visited ++ [name h]
  in if accepts h amount
       then (visited', name h, "refund(" ++ show amount ++ ")")
       else handle amount rest visited'

main :: IO ()
main = do
  let (visited, handled, result) = handle 250 handlers []
  putStrLn $ "visited=" ++ joinWith ">" visited ++ ";handled=" ++ handled ++ ";result=" ++ result

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x:xs) = x ++ sep ++ joinWith sep xs
