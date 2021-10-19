module Main where

import Find
import System.Console.ArgParser
import Control.Applicative
import System.IO
import System.Console.ANSI
import Data.List.Split
import Data.List (intercalate)

findParser :: ParserSpec Find
findParser = File
  `parsedBy` reqPos "search string" `Descr` "description for the first argument"
  `andBy` reqPos "file name" `Descr` "description for the second argument"
  `andBy` boolFlag "color" `Descr` "prints the output in color"
  
findApp :: IO (CmdLnInterface Find)
findApp = (`setAppDescr` "top description")
  <$> (`setAppEpilog` "bottom description")
  <$> mkApp findParser
  
main :: IO ()
main = findApp >>= \x -> runApp x find

find :: Find -> IO ()
find (File search file True) = openFile file ReadMode >>= hGetContents >>=
  printResults search
find (File search file False) = openFile file ReadMode >>= hGetContents >>=
  sequence_ . map (\(number, line) -> putStrLn (show number ++ ": " ++ line)) . searchFile search

printResults :: String -> String -> IO ()
printResults searchString = sequence_
  . map (printOutput searchString)
  . (searchFile searchString)

printOutput :: String -> (Int, String) -> IO ()
printOutput search (lineNumber, line) =
  setSGR [SetColor Foreground Vivid Red] >>
  (putStr $ show lineNumber) >>
  putStr ": " >>
  sequence_ (printLine search line) >>
  setSGR [] >>
  putStrLn "" >>
  hFlush stdout

printLine :: String -> String -> [IO ()]
printLine search line =
  intercalate ([setSGR [SetColor Foreground Vivid Blue], putStr search])
  (fmap (\x -> [setSGR [], putStr x]) (splitOn search line))
