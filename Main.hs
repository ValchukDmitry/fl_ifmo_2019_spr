module Main where

import Tokenizer

runTokenizer :: String -> IO ()
runTokenizer input = do
  putStrLn input
  putStrLn $ show $ tokenize input
  putStrLn ""

main :: IO ()
main = do
  runTokenizer " 1 2 abc if "
  runTokenizer " "


testParser :: Bool -> String -> [Token] -> String
testParser shouldFail s tokens = let result = tokenize s in
  if (result == tokens) `xor` shouldFail
    then "PASS"
    else "FAIL " ++ s ++ "; Expected: " ++ show tokens ++ "; Actual: " ++ show result

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

validTests = [
  ("1234", [Number 1234]),
  ("abra", [Ident "abra"]),
  ("for", [KeyWord "for"]),
  ("hello world", [Ident "hello", Ident "world"]),
  ("1234 hello", [Number 1234, Ident "hello"]),
  ("for if 1234 1234          hello", [KeyWord "for", KeyWord "if", Number (1234), Number 1234, Ident "hello"]),
  ("1 2 33 4   2222222", [Number 1, Number 2, Number (33), Number 4, Number 2222222]),
  ("0", [Number 0]),
  ("_hello", [Ident "_hello"])]

invalidTests = [("should fail", [KeyWord "should", Ident "fail"])]

runTests = (uncurry (testParser False) <$> validTests) ++
          (uncurry (testParser True) <$> invalidTests)
