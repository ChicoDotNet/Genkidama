module Main where

import Data.List (intercalate)

data Builder = Builder
  { addTitle :: String -> [String] -> [String]
  , addSection :: String -> String -> [String] -> [String]
  , render :: [String] -> String
  }

textBuilder :: Builder
textBuilder = Builder
  { addTitle = \title parts -> parts ++ ["# " ++ title]
  , addSection = \heading body parts -> parts ++ ["## " ++ heading, body]
  , render = intercalate "\n"
  }

htmlBuilder :: Builder
htmlBuilder = Builder
  { addTitle = \title parts -> parts ++ ["<h1>" ++ title ++ "</h1>"]
  , addSection = \heading body parts -> parts ++ ["<h2>" ++ heading ++ "</h2>", "<p>" ++ body ++ "</p>"]
  , render = concat
  }

buildAvailabilityReport :: Builder -> String
buildAvailabilityReport builder =
  let withTitle = addTitle builder "Service status" []
      complete = addSection builder "Availability" "99.95%" withTitle
  in render builder complete

main :: IO ()
main = do
  putStrLn (buildAvailabilityReport textBuilder)
  putStrLn "---"
  putStrLn (buildAvailabilityReport htmlBuilder)
