module Main where

import System.Environment
import Expression
import Text.Printf

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseExpression input
        let r = executeExpression input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either (concatMap show) show a
        putStrLn $ either (concatMap show) show r
        putStrLn ""
    )
    fileNames
