data Node
  = File Int
  | Folder [Node]

size :: Node -> Int
size (File bytes) = bytes
size (Folder children) = sum (map size children)

main :: IO ()
main = do
  let readme = File 2
      docs = Folder [File 3, File 5]
      root = Folder [readme, docs]
  putStrLn ("leaf=" ++ show (size readme))
  putStrLn ("docs=" ++ show (size docs))
  putStrLn ("root=" ++ show (size root))
