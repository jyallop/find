module Search.Find where

data Find =
    Contents Bool String String
  | File String String
  deriving (Show, Eq)

searchFile :: String -> String -> [(Int, String)]
searchFile searchString =
  filter (\(_, line) -> searchString `elem` (words line)) . zip [1..] . lines

