module Automaton where

import Combinators

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Char
import Data.List (sort, union, nub, (\\), intersect, delete)
import Data.Maybe (isNothing)

type Set = Set.Set
type Map = Map.Map

data Delta s q = Delta q (Maybe s) (Maybe q) deriving Show

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

epsilonParser = keywordsWithDelims checkDelimiter ["\\epsilon"]

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

            deltas <- automatonPartParser (deltaInnerParser elemsParser epsilonParser) (>=0)
            let delta = (\[x0, x1, x2] ->
                    if x1 == "\\epsilon"
                        then Delta x0 Nothing (Just x2)
                        else Delta x0 (Just x1) (Just x2)) <$>
                        deltas
            spaces
            end
            if checkDeltas states sigmas deltas then
                return (Automaton sigma state (head initStates) termState delta)
            else
                fail $ "bad deltas"

checkDeltas states sigmas [] = True
checkDeltas states sigmas ((s1:c:s2:[]):xs) = elem s1 states &&
                                            elem s2 states &&
                                            (elem c sigmas || c == "\\epsilon") &&
                                            checkDeltas states sigmas xs

isDFA :: (Eq s, Eq q) => Automaton s q -> Bool
isDFA (Automaton _ _ _ _ deltas) = checkUnique deltas &&
                                    foldr (\(Delta f c t) r -> r && (not $ isNothing c)) True deltas
    where
    checkUnique :: (Eq s, Eq q) => [Delta s q] -> Bool
    checkUnique [] = True
    checkUnique (x:xs) = if x `elem` xs then False else checkUnique xs
isNFA :: (Eq s, Eq q) => Automaton s q -> Bool
isNFA = const True

isComplete :: (Eq s, Eq q) => Automaton s q -> Bool
isComplete a@(Automaton sigma states _ _ deltas) = if not (isDFA a) then False else and $
                                                    flip elem deltas <$> -- Mask of existing delta
                                                    [(Delta y (Just x) Nothing) |
                                                    x <- Set.elems sigma,
                                                    y <- Set.elems states] -- all posible deltas

isMinimal = const True

makeComplete :: Automaton String String -> Automaton String String
makeComplete a@(Automaton sigma stts initState termState delta) | isComplete a = a
                                                                | otherwise =
    let newStates = (devilState stts) `Set.insert` stts in
        Automaton sigma newStates initState termState (calcNewDelta newStates delta (devilState stts))
        where
            devilState st = '_':(concat st)
            calcNewDelta :: Set String -> [Delta String String] -> String -> [Delta String String]
            calcNewDelta st' delta devilState =
                delta `union` [(Delta y (Just x) (Just devilState)) | x <- Set.elems sigma, y <- Set.elems st']

makeDFA :: Automaton String String -> Automaton String String
makeDFA a@(Automaton sigma stts initState termState delta) | isDFA a = a
                                                           | otherwise =
    let states = []
        deltas = []
        newInitState = initState
        termState = []
        (newStates, newTermStates, newDelta) = processUnprocessed [[initState]] [(Set.elems stts)] termState delta [] [[initState]]
    in
        Automaton sigma (Set.fromList newStates) initState (Set.fromList newTermStates) newDelta
         where

            processUnprocessed :: [[String]] -> [[String]] -> [String] -> [Delta String String] -> [Delta String [String]] -> [[String]] -> ([String], [String], [Delta String String])
            processUnprocessed [] states termState delta newDelta newStates =
                (map (\x -> concat x) newStates, getTermStates termState newStates, map (\(Delta f (Just c) (Just t)) -> Delta (concat f) (Just c) (Just $ concat t)) newDelta)

            processUnprocessed (st:sts) states termState delta newDelta newStates =
                let (currentNewStates, newDeltas) = processState st st delta ([], [])
                    actuallyNew = (currentNewStates \\ newStates)
                    actuallyNewDeltas = (newDelta \\ newDeltas) in
                        processUnprocessed (sts ++ actuallyNew) states termState delta (nub $ newDelta ++ actuallyNewDeltas) (nub $ newStates ++ actuallyNew)

            getTermStates term newStates = map (\x -> concat x) $ filter (\x -> (length $ x `intersect` term) > 1) newStates

            processState :: [String] -> [String] -> [Delta String String] -> ([[String]], [Delta String [String]]) -> ([[String]], [Delta String [String]])
            processState startState [] delta result = result
            processState startState (s:st) delta result =
                let currentState = delete s startState
                    (resultStates, resultDelta) = result
                    newStates = nub $ map (\(Delta f (Just c) (Just t)) -> (sort (nub $ t:currentState))) $ filter (\(Delta f c t) -> f==s) delta
                    newDelta = map (\(Delta f (Just c) (Just t)) -> (Delta startState (Just c) (Just (sort (nub $ t:currentState))))) $ filter (\(Delta f c t) -> f == s) delta
                in
                    processState startState st delta ((nub $ (resultStates ++ newStates)), newDelta)

epsClosure :: (Eq s, Eq q) => Automaton s q -> Automaton s q
epsClosure a@(Automaton sigma stts initState termState delta) | foldr (\(Delta f c t) r -> r && (not $ isNothing c)) True delta == True = a
                                                              | otherwise =
    let epsEdges = filter (\(Delta f c t) -> (isNothing c)) delta
    in
        Automaton sigma stts initState termState (nub $ calcEpsDelta (Set.elems stts) epsEdges delta)
    where
        calcEpsDelta [] epsilonEdges delta = []
        calcEpsDelta allStates@(s:sts) epsilonEdges delta = nub ((map (\(Delta f c t) -> Delta s c t) $
            getNextState allStates epsilonEdges $
            map (\(Delta _ _ (Just t)) -> t) $ filter (\(Delta f _ _) -> f==s) epsilonEdges)
            ++ calcEpsDelta sts epsilonEdges delta)
        getNextState states edges epsilonStates = filter (\(Delta f c (Just t)) -> f `elem` epsilonStates) edges



