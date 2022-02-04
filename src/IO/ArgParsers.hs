module IO.ArgParsers where

import Search.Find
import System.Console.ArgParser
import Control.Applicative

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

