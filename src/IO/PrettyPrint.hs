module IO.PrettyPrint where

import Data.List.Split
import System.Console.ANSI
import System.IO
import Data.List (intercalate)

printResults :: String -> [(Int, String)] -> IO ()
printResults searchString results = sequence_
  $ map (printOutput searchString) results

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
