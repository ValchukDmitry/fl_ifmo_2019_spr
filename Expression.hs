module Expression where

import Text.Printf
import Combinators
import Data.Bits

data Operator = Pow
              | Mul
              | Div
              | Sum
              | Minus
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              | Conj
              | Disj

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Primary a

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either [ParsingError String] (EAst Integer)
parseExpression input = runParserUntilEof (expression operatorsAST (Primary <$> intParser)) input

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either [ParsingError String] Integer
executeExpression input =
  runParserUntilEof (expression operatorsCalc intParser) input

instance Show Operator where
  show Pow   = "^"
  show Mul   = "*"
  show Div   = "/"
  show Sum   = "+"
  show Minus = "-"
  show Eq    = "=="
  show Neq   = "/="
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"
  show Conj  = "&&"
  show Disj  = "||"

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (Prelude.show op) (show' (ident n) l) (show' (ident n) r)
                  Primary x -> Prelude.show x)
      ident = (+1)

{-
show (BinOp Conj (BinOp Pow (Primary 1) (BinOp Sum (Primary 2) (Primary 3))) (Primary 4))

&&
|_^
| |_1
| |_+
| | |_2
| | |_3
|_4
-}


operatorsAST =
  [
    (RAssoc, [ (string "||", BinOp Disj)]),
    (RAssoc, [ (string "&&", BinOp Conj)]),
    (NAssoc, [
      (string "<=", BinOp Le),
      (string ">=", BinOp Ge),
      (string "==", BinOp Eq),
      (string "!=", BinOp Neq),
      (string "<", BinOp Lt),
      (string ">", BinOp Gt)
    ]),
    (LAssoc, [
      (string "+", BinOp Sum),
      (string "-", BinOp Minus)
    ]),
    (LAssoc, [
      (string "*", BinOp Mul),
      (string "/", BinOp Div)
    ]),
    (RAssoc, [ (string "^", BinOp Pow) ])
  ]

predicate :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Integer
predicate f a b = if f a b then 1 else 0

operatorsCalc =
  [
    (RAssoc, [ (string "||", (.|.)) ]),
    (RAssoc, [ (string "&&", (.&.)) ]),
    (NAssoc, [
      (string "==", predicate (==)),
      (string "!=", predicate (/=)),
      (string "<=", predicate (<=)),
      (string "<", predicate (<)),
      (string ">=", predicate (>=)),
      (string ">", predicate (>))
    ]),
    (LAssoc, [
      (string "+", (+)),
      (string "-", (-))
    ]),
    (LAssoc, [
      (string "*", (*)),
      (string "/", div)
    ]),
    (RAssoc, [ (string "^", (^)) ])
  ]