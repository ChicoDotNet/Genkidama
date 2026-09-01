module Memento
  ( Document (..),
    MementoSnapshot (..),
    save,
    restore,
    verifyMementoCanonical,
    main,
  )
where

data Document = Document
  { documentText :: String
  }
  deriving (Eq, Show)

newtype MementoSnapshot = MementoSnapshot String
  deriving (Eq, Show)

save :: Document -> MementoSnapshot
save = MementoSnapshot . documentText

restore :: Document -> MementoSnapshot -> Document
restore document (MementoSnapshot text) = document {documentText = text}

verifyMementoCanonical :: Bool
verifyMementoCanonical =
  let draft = Document "draft"
      snapshot = save draft
      published = draft {documentText = "published"}
      restored = restore published snapshot
   in documentText published == "published"
        && documentText restored == "draft"
        && snapshot == MementoSnapshot "draft"

main :: IO ()
main =
  if verifyMementoCanonical
    then putStrLn "Haskell Memento: passed"
    else error "Haskell Memento: failed"
