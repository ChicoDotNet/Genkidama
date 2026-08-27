import qualified Data.Map.Strict as M

data TextStyle = TextStyle String Int String deriving (Eq, Show)
type Pool = M.Map (String, Int, String) TextStyle

intern :: Pool -> (String, Int, String) -> (TextStyle, Pool)
intern pool key@(font, size, color) =
  case M.lookup key pool of
    Just style -> (style, pool)
    Nothing -> let style = TextStyle font size color in (style, M.insert key style pool)

main :: IO ()
main = do
  let (red1, p1) = intern M.empty ("Inter", 12, "red")
      (red2, p2) = intern p1 ("Inter", 12, "red")
      (_, p3) = intern p2 ("Inter", 12, "blue")
      shared = red1 == red2
  putStrLn $ "styles=" ++ show (M.size p3) ++ ";shared=" ++ map toLowerBool (show shared) ++ ";text=ABC"
  where
    toLowerBool 'T' = 't'
    toLowerBool 'F' = 'f'
    toLowerBool c = c
