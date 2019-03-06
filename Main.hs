module Main where

import System.Environment
import Automaton
import Text.Printf

automatonInfo :: (Eq a, Eq b) => Automaton a b -> String
automatonInfo auto =
  let [dfa, nfa, complete, minimal] = map (\f -> if f auto then "yes" else "no") [isDFA, isNFA, isComplete, isMinimal] in
  printf "Hurray! It's an automaton!\nDeterministic:    %s\nNondeterministic: %s\nComplete:         %s\nMinimal:          %s" dfa nfa complete minimal

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseAutomaton input
        putStrLn $ printf "Parsing %s\n" fileName
        let result = case a of
                      Right a -> automatonInfo a
                      Left err -> show err
        putStrLn result
        putStrLn ""
    )
    fileNames