module Automaton where

import Combinators

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Char
import Data.List (sort)

type Set = Set.Set
type Map = Map.Map

data Delta s q = Delta q s q deriving Show

instance (Eq s, Eq q) => Eq (Delta s q) where
    (Delta f s t) == (Delta f' s' t') = f==f' && s==s'

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: [Delta s q]
                               } deriving Show

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q
delimiterParser = satisfy (==',')

bracketsAndDelimiters = [',', '(', ')', '<', '>']
checkDelimiter = (\x -> (isSpace x) || (elem x bracketsAndDelimiters))
elemsParser = some $ satisfy (\x -> not (isSpace x) && (not $ elem x bracketsAndDelimiters))
automatonPartParser elem minCount = parseList elem (satisfy (==',')) (satisfy (=='<')) (satisfy (=='>')) minCount
nextPart = (many $ satisfy isSpace) *> satisfy (==',') <* (many $ satisfy isSpace)

deltaInnerParser elem1 elem2 = parseList (elem1 <|> elem2) (satisfy (==',')) (satisfy (=='(')) (satisfy (==')')) (==3)

parseAutomaton :: String -> Either [ParsingError String] (Automaton String String)
parseAutomaton s = snd <$> runParser parserAutomaton (initStream s)
    where
        parserAutomaton = do
            spaces
            sigmas <- automatonPartParser elemsParser (>=1)
            let sigma = Set.fromList sigmas
            nextPart
            states <- automatonPartParser elemsParser (>=1)
            let state = Set.fromList states
            nextPart
            initStates <- automatonPartParser (keywordsWithDelims checkDelimiter states) (==1)
            let initState = Set.fromList initStates
            nextPart
            termStates <- automatonPartParser (keywordsWithDelims checkDelimiter states) (>=0)
            let termState = Set.fromList termStates
            nextPart

            deltas <- automatonPartParser (deltaInnerParser elemsParser elemsParser) (>=0)
            let delta = (\[x0, x1, x2] -> Delta x0 x1 x2) <$> deltas
            spaces
            end
            if checkDeltas states sigmas deltas then
                return (Automaton sigma state (head initStates) termState delta)
            else
                fail $ "bad deltas"

checkDeltas states sigmas [] = True
checkDeltas states sigmas ((s1:c:s2:[]):xs) = elem s1 states && elem s2 states && elem c sigmas && checkDeltas states sigmas xs

isDFA :: (Eq s, Eq q) => Automaton s q -> Bool
isDFA (Automaton _ _ _ _ deltas) = checkUnique deltas where
    checkUnique :: (Eq s, Eq q) => [Delta s q] -> Bool
    checkUnique [] = True
    checkUnique (x:xs) = if x `elem` xs then False else checkUnique xs
isNFA :: (Eq s, Eq q) => Automaton s q -> Bool
isNFA = not . isDFA

isComplete :: (Eq s, Eq q) => Automaton s q -> Bool
isComplete (Automaton sigma states _ _ deltas) = and $
                                                    flip elem deltas <$> -- Mask of existing delta
                                                    [(Delta y x y) |
                                                    x <- Set.elems sigma,
                                                    y <- Set.elems states] -- all posible deltas

isMinimal = const True


