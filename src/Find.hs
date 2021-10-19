module Find where

data Find =
  File String String
  deriving (Show, Eq)

searchFile :: String -> String -> [(Int, String)]
searchFile searchString = filter (\(_, line) -> searchString `elem` (words line)) . zip [1..] . lines
