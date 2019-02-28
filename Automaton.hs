module Automaton where

import Combinators

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Char

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Maybe q)
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
elems = some $ satisfy (\x -> not (isSpace x) && (not $ elem x bracketsAndDelimiters))
automatonPartParser elem minCount = parseList elem (satisfy (==',')) (satisfy (=='<')) (satisfy (=='>')) minCount
nextPart = (many $ satisfy isSpace) *> satisfy (==',') <* (many $ satisfy isSpace)

deltaInnerParser elem1 elem2 = parseList (elem1 <|> elem2) (satisfy (==',')) (satisfy (=='(')) (satisfy (==')')) (==3)

parseAutomaton :: String -> Maybe (Automaton String String)
parseAutomaton s = snd <$> (runParser parserAutomaton s) where
    parserAutomaton = do
        sigmas <- automatonPartParser elems (>=1)
        let sigma = Set.fromList sigmas
        nextPart
        states <- automatonPartParser elems (>=1)
        let state = Set.fromList states
        nextPart
        initStates <- automatonPartParser (keywordsWithDelims checkDelimiter states) (==1)
        let initState = Set.fromList initStates
        nextPart
        termStates <- automatonPartParser (keywordsWithDelims checkDelimiter states) (>=0)
        let termState = Set.fromList termStates
        nextPart

        deltas <- automatonPartParser (deltaInnerParser elems elems) (>=0)
        let delta = Map.fromList $ (\[x0, x1, x2] -> ((x0, x1), Just x2)) <$> deltas
        end
        if checkDeltas states sigmas deltas then
            return (Automaton sigma state (head initStates) termState delta)
        else
            Combinators.fail

checkDeltas states sigmas [] = True
checkDeltas states sigmas ((s1:c:s2:[]):xs) = elem s1 states && elem s2 states && elem c sigmas && checkDeltas states sigmas xs
