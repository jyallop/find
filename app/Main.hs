module Main where

import Search.Find
import IO.PrettyPrint
import System.Console.ArgParser
import Control.Applicative
import System.Directory
import System.IO

findParser :: IO (CmdLnInterface Find)
findParser = mkSubParser
  [
    ("file", setAppDescr (mkDefaultApp fileParser "file")
             "searches the given directory for a file named by the search string"),
    ("contents", setAppDescr (mkDefaultApp contentsParser "contents")
                 "searches the given file for occurences of the search string")
  ]

contentsParser :: ParserSpec Find
contentsParser = Contents `parsedBy` boolFlag "color" `Descr` "prints the output in color"
  `andBy` reqPos "file" `Descr` "the file to search for"
  `andBy` reqPos "search-string" `Descr` "file name to search for"

fileParser :: ParserSpec Find
fileParser = File `parsedBy` reqPos "directory" `Descr` "the directory to search"
  `andBy` reqPos "search-string" `Descr` "search string to look for"

main :: IO ()
main = findParser >>= \x -> runApp x find

find :: Find -> IO ()
find (File dir search) = getDirectoryContents dir >>= putStrLn . show
find (Contents True file search) = openFile file ReadMode >>= hGetContents >>=
  (printResults . (searchFile search))
find (Contents False file search) = openFile file ReadMode >>= hGetContents >>=
  sequence_ . map (\(number, line) -> putStrLn (show number ++ ": " ++ line)) . searchFile search
