type Component = String

plain :: Component
plain = "alert"

audit :: Component -> Component
audit inner = "audit(" ++ inner ++ ")"

encrypt :: Component -> Component
encrypt inner = "enc(" ++ inner ++ ")"

main :: IO ()
main = do
  putStrLn $ "base=" ++ plain
  putStrLn $ "audit=" ++ audit plain
  putStrLn $ "encrypted=" ++ encrypt plain
  putStrLn $ "stacked=" ++ audit (encrypt plain)
