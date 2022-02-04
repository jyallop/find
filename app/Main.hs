module Main where

import Search.Find
import IO.PrettyPrint
import System.Console.ArgParser
import System.Directory
import System.IO
import IO.ArgParsers

main :: IO ()
main = findParser >>= \x -> runApp x find

find :: Find -> IO ()
find (File dir search) = getDirectoryContents dir >>= putStrLn . show
find (Contents True file search) = openFile file ReadMode >>= hGetContents >>=
  printResults search . searchFile search 
find (Contents False file search) = openFile file ReadMode >>= hGetContents >>=
  sequence_ . map (\(number, line) -> putStrLn (show number ++ ": " ++ line)) . searchFile search
