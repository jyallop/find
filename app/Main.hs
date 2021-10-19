module Main where

import Lib
import System.Console.ArgParser
import Control.Applicative
import System.IO

data Find =
  File String String
  deriving (Show, Eq)

findParser :: ParserSpec Find
findParser = File
  `parsedBy` reqPos "search string" `Descr` "description for the first argument"
  `andBy` reqPos "file name" `Descr` "description for the second argument"

findApp :: IO (CmdLnInterface Find)
findApp = (`setAppDescr` "top description")
  <$> (`setAppEpilog` "bottom description")
  <$> mkApp findParser
  
main :: IO ()
main = findApp >>= \x -> runApp x find

find :: Find -> IO ()
find (File search file) = openFile file ReadMode >>= hGetContents >>=
  printResults search >> putStrLn "Done"

printResults :: String -> String -> IO [()]
printResults searchString = sequence
  . (map (\(lineNumber, line) -> putStrLn (foldl (++) (show lineNumber) [": ", line])))
  . (searchFile searchString)

searchFile :: String -> String -> [(Int, String)]
searchFile searchString = filter (\(_, line) -> searchString `elem` (words line)) . zip [1..] . lines
