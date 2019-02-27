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
string = some $ satisfy isLetter
automatonPartParser elem minCount = parseList elem (satisfy (==',')) (satisfy (=='<')) (satisfy (=='>')) minCount
nextPart = satisfy (==',')

deltaInnerParser elem1 elem2 = parseList (elem1 <|> elem2) (satisfy (==',')) (satisfy (=='(')) (satisfy (==')')) 3

parseAutomaton :: String -> Maybe (Automaton String String)
parseAutomaton s = snd <$> (runParser parserAutomaton s) where
    parserAutomaton = do
        sigmas <- automatonPartParser string 1
        let sigma = Set.fromList sigmas
        nextPart
        states <- automatonPartParser string 1
        let state = Set.fromList states
        nextPart
        initStates <- automatonPartParser string 1
        let initState = Set.fromList initStates
        nextPart
        termStates <- automatonPartParser string 0
        let termState = Set.fromList termStates
        nextPart

        deltas <- automatonPartParser (deltaInnerParser string string) 0
        let deltaK1 = (\x-> x!!0) <$> deltas
        let deltaK2 = (\x-> x!!1) <$> deltas
        let deltaValue = (\x-> Just $ x!!2) <$> deltas

        let delta = Map.fromList $ zip (zip deltaK1 deltaK2) deltaValue
        return (Automaton sigma state (head initStates) termState delta)

