module Combinators where

  import qualified Prelude
  import Prelude hiding (fail, fmap, (<*>), (>>=))
  import Control.Applicative (Alternative)
  import qualified Control.Applicative (empty, (<|>))


  -- Parsing result is some payload and a suffix of the input which is yet to be parsed
  newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

  -- Parser which always succeedes consuming no input
  success :: ok -> Parser str ok
  success ok = Parser $ \s -> Just (s, ok)

  -- Parser which fails no mater the input
  fail :: Parser str ok
  fail = Parser $ const Nothing

  -- Biased choice: if the first parser succeedes, the second is never run
  (<|>) :: Parser str ok -> Parser str ok -> Parser str ok
  p <|> q = Parser $ \s ->
    case runParser p s of
      Nothing -> runParser q s
      x -> x

  -- Default sequence combinator
  -- If the first parser succeedes then the second parser is used
  -- If the first does not succeed then the second one is never tried
  -- The result is collected into a pair
  seq :: Parser str a -> Parser str b -> Parser str (a, b)
  p `seq` q = Parser $ \s ->
    case runParser p s of
      Nothing -> Nothing
      Just (s', x) -> case runParser q s' of
        Nothing -> Nothing
        Just (s'', x') -> Just (s'', (x, x'))


  -- Monadic sequence combinator
  (>>=) :: Parser str a -> (a -> Parser str b) -> Parser str b
  p >>= q = Parser $ \s ->
    case runParser p s of
      Nothing -> Nothing
      Just (s', x) -> case runParser (q x) s' of
        Nothing -> Nothing
        Just (s'', x') -> Just (s'', x')

  -- Applicative sequence combinator
  (<*>) :: Parser str (a -> b) -> Parser str a -> Parser str b
  p <*> q = Parser $ \s ->
    case runParser p s of
      Nothing -> Nothing
      Just (s', x) -> case runParser q s' of
        Nothing -> Nothing
        Just (s'', x') -> Just (s'', x x')


  -- Applies a function to the parsing result, if parser succeedes
  fmap :: (a -> b) -> Parser str a -> Parser str b
  fmap f p = Parser $ \s ->
    case runParser p s of
      Nothing -> Nothing
      Just (s', a) -> Just (s', f a)

  -- Applies a parser once or more times
  some :: Parser str a -> Parser str [a]
  some p = ((:) <$> p) <*> many p

  -- Applies a parser zero or more times
  many :: Parser str a -> Parser str [a]
  many p = manyInner where
    manyInner = someInner <|> success []
    someInner = ((:) <$> p) <*> manyInner

  -- Parses keywords

  buildTrie kws = foldl insert (Trie False []) kws

  keywords :: [String] -> Parser String String
  keywords kws = Parser $ \s -> process s (buildTrie kws) ""
      where
        process [] (Trie True _) str = Just ([], str)
        process [] _ _ = Nothing
        process (' ':xs) (Trie True _) str = Just (' ':xs, str)
        process (' ':xs) _ _ = Nothing
        process (x:xs) trie str = case step trie x of
                                           Nothing -> Nothing
                                           Just ve -> process xs ve (str ++ [x])

  data Trie key = Trie Bool [(key, Trie key)]

  empty = Trie False []

  insert :: (Eq k) => Trie k -> [k] -> Trie k
  insert (Trie _ edges) []     = Trie True edges
  insert (Trie t edges) (x:xs) = case lookup x edges of
                                  Nothing -> Trie t $ (x, insert empty xs):edges
                                  Just ve -> Trie t (insertEdge <$> edges)
                                  where
                                    insertEdge (xkey, trie) =
                                      if xkey == x then (xkey, insert trie xs) else (xkey, trie)

  step (Trie _ edges) key = lookup key edges

  instance Prelude.Functor (Parser s) where
    fmap = fmap

  instance Prelude.Applicative (Parser s) where
    pure = success
    (<*>) = (<*>)

  instance Alternative (Parser s) where
    empty = fail
    (<|>) = (<|>)

  instance Prelude.Monad (Parser s) where
    return = pure
    (>>=) = (>>=)
    fail = const fail

  satisfy :: (token -> Bool) -> Parser [token] token
  satisfy pr = Parser f
    where
      f (c:cs) | pr c  = Just (cs,c)
      f _              = Nothing

  spaces = many (satisfy (==' '))
  parseList :: Parser String elem -> Parser String delim -> Parser String lbr -> Parser String rbr -> Int -> Parser String [elem]
  parseList elem delim lbr rbr minCount =
    let parseItem = spaces *> elem <* (spaces) <* delim in
    let parseLast = spaces *> elem <* spaces in
    let parseItems = many parseItem in
      do
        lbr
        items <- parseItems
        last <- parseLast
        rbr
        if length items >= (minCount - 1) then
          return $ items ++ [last]
        else
          fail


