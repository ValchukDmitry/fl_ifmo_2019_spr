module Combinators where
  import Data.Char (isSpace, isDigit, digitToInt)
  import Data.Foldable (toList)
  import qualified Prelude
  import Prelude hiding ((<*>), (>>=))
  import Control.Applicative (Alternative)
  import qualified Control.Applicative (empty, (<|>))
  import Debug.Trace (trace)

  -- Parsing result is either an error message or some payload and a suffix of the input which is yet to be parsed
  newtype Parser str ok = Parser { runParser :: Stream str -> Either [ParsingError String] (Stream str, ok) }

  data Assoc = LAssoc -- left associativity
            | RAssoc -- right associativity
            | NAssoc -- not associative

  -- General parser combinator for expressions
  -- Binary operators are listed in the order of precedence (from lower to higher)
  -- Binary operators on the same level of precedence have the same associativity
  -- Binary operator is specified with a parser for the operator itself and a semantic function to apply to the operands
  expression :: [(Assoc, [(Parser str b, a -> a -> a)])] ->
                Parser str a ->
                Parser str a
  expression ops primary = foldr (\(associativity, operators) acc -> process associativity operators acc) primary ops
    where
      process:: Assoc -> [(Parser str b, a -> a -> a)] -> Parser str a -> Parser str a
      process associativity = case associativity of
          LAssoc -> processLeft
          RAssoc -> processRight
          NAssoc -> processNvm

      processLeft :: [(Parser str b, a -> a -> a)] -> Parser str a -> Parser str a
      processLeft operators primary = do
          left <- primary
          let constr = fmap (\(parser, op) -> parser *> pure op) operators
          let constr' = foldr1 (\parser acc -> parser <|> acc) constr
          right <- many $ fmap flip constr' <*> primary
          return $ foldl (flip ($)) left right

      processRight :: [(Parser str b, a -> a -> a)] -> Parser str a -> Parser str a
      processRight operators primary = do
          left <- primary
          let constr = fmap (\(parser, op) -> parser *> pure op) operators
          let constr' = foldr1 (<|>) constr
          op <- constr'
          right <- processRight operators primary
          return (op left right)
          <|> primary

      processNvm :: [(Parser str b, a -> a -> a)] -> Parser str a -> Parser str a
      processNvm operators primary = do
          left <- primary
          let constr = fmap (\(parser, op) -> parser *> pure op) operators
          let constr' = foldr1 (<|>) constr
          op <- constr'
          right <- primary
          return (op left right)
          <|> primary


  runParserUntilEof :: Foldable t => Parser str ok -> t str -> Either [ParsingError String] ok
  runParserUntilEof p inp =
    either (Left . id) (\(str, ok) -> if (null (stream str))
                                        then Right ok
                                        else (Left $ buildError str "Expected eof")) (runParser p (initStream inp))

  -- Parsing result is some payload and a suffix of the input which is yet to be parsed
  data Location = Location {line::Int, column::Int} deriving Show
  data Stream token = Stream {stream::[token], position::Location} deriving Show

  initStream :: Foldable t => t token -> Stream token
  initStream token = Stream (toList token) (Location 1 1)

  next :: Stream token -> Maybe token
  next (Stream [] _) = Nothing
  next (Stream (x:xs) _) = Just x

  streamStep :: Stream Char -> Stream Char
  streamStep (Stream (x:xs) (Location l c)) | x == '\n' = Stream xs (Location (l+1) 1)
                                            | otherwise = Stream xs (Location l (c+1))
  streamStep (Stream [] loc) = Stream [] loc

  data ParsingError cause = ParseError {location::Location, cause::cause} deriving Show
  buildError :: Stream tok -> cause -> [ParsingError cause]
  buildError stream cause = [ParseError (position stream) cause]

  -- Parser which always succeedes consuming no input
  success :: ok -> Parser str ok
  success ok = Parser $ \s -> Right (s, ok)

  -- Biased choice: if the first parser succeedes, the second is never run
  (<|>) :: Parser str ok -> Parser str ok -> Parser str ok
  p <|> q = Parser $ \s ->
    case runParser p s of
      Left e -> case runParser q s of
        Left e' -> Left $ e ++ e'
        x -> x
      x -> x


  -- Monadic sequence combinator
  (>>=) :: Parser str a -> (a -> Parser str b) -> Parser str b
  p >>= q = Parser $ \s -> case runParser p s of
    Left e -> Left e
    Right (s', a) -> case runParser (q a) s' of
        Left e -> Left e
        Right (s'', b) -> Right (s'', b)

  -- Applicative sequence combinator
  (<*>) :: Parser str (a -> b) -> Parser str a -> Parser str b
  p <*> q = Parser $ \s -> case runParser p s of
    Left  e -> Left e
    Right (s', f) -> case runParser q s' of
        Left  e -> Left e
        Right (s'', a) -> Right (s'', f a)

  -- Applies a function to the parsing result, if parser succeedes
  -- fmap :: (a -> b) -> Parser str a -> Parser str b
  -- fmap f p = Parser $ \s -> case runParser p s of
  --   Left e -> Left e
  --   Right (s', a) -> Right (s', f a)

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

  keywords :: [String] -> Parser Char String
  keywords = keywordsWithDelims (==' ')

  keywordsWithDelims :: (Char -> Bool) -> [String] -> Parser Char String
  keywordsWithDelims isDelim kws = Parser $ \s -> process s (buildTrie kws) ""
      where
        process s@(Stream [] loc) (Trie True _) str = Right (s, str)
        process s@(Stream [] loc) _ _ = Left $ unexpectedToken s
        process s@(Stream (x:xs) loc) trie str  | isDelim x = processDelims s trie str
                                                | otherwise = case step trie x of
                                                                Nothing -> Left $ unexpectedToken s
                                                                Just ve -> process (streamStep s) ve (str ++ [x])
          where
            processDelims stream (Trie True _) str = Right (stream, str)
            processDelims stream _ _ = Left $ unexpectedToken stream

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
    fmap f p = Parser $ \s -> case runParser p s of
      Left e -> Left e
      Right (s', a) -> Right (s', f a)

  instance Prelude.Applicative (Parser s) where
    pure = success
    (<*>) = (<*>)

  instance Alternative (Parser s) where
    empty = Parser $ \s -> Left $ buildError s "unknown error"
    (<|>) = (<|>)

  instance Prelude.Monad (Parser s) where
    return = pure
    (>>=) = (>>=)
    -- fail :: String -> Parser s a
    fail cause = Parser $ \s -> Left $ buildError s cause

  unexpectedToken :: Show token => (Stream token) -> [ParsingError String]
  unexpectedToken s@(Stream (t:ts) loc) = buildError s $ "unexpected token: {" ++ show t ++ "}"
  unexpectedToken s@(Stream [] loc) = buildError s $ "unexpected eof"

  satisfy :: (Char -> Bool) -> Parser Char Char
  satisfy pr = Parser f
    where
      f s = let cur = next s in
        case cur of
          Nothing -> Left $ unexpectedToken s
          Just c -> case pr c of
                      True -> Right (streamStep s, c)
                      False -> Left $ unexpectedToken s

  end :: Show token => Parser token ()
  end = Parser $ \s -> case s of
    (Stream [] _) -> Right (s, ())
    _  -> Left $ unexpectedToken s

  try :: Parser Char a -> Parser Char (Maybe a)
  try prs = fmap Just prs <|> success Nothing

  spaces = many (satisfy isSpace)
  parseList ::  Parser Char elem ->
                Parser Char delim ->
                Parser Char lbr ->
                Parser Char rbr ->
                (Int -> Bool) ->
                Parser Char [elem]
  parseList elem delim lbr rbr countPredicate =
    let parseItem = spaces *> elem <* (spaces) <* delim in
    let parseLast = spaces *> elem <* spaces in
    let parseItems = many parseItem in
      do
        lbr
        items <- parseItems
        last <- try parseLast
        rbr
        let res = case last of
                    Nothing -> items
                    Just x -> items ++ [x]

        if countPredicate (length res) then
          return res
        else
          fail $ "bad list size: " ++ show (length res)

  char :: Char -> Parser Char Char
  char letter = satisfy (== letter)

  string :: String -> Parser Char String
  string = foldr (\a b -> pure (:) <*> char a <*> b) (pure [])

  digit :: Parser Char Char
  digit = satisfy (isDigit)

  digitInt :: Parser Char Integer
  digitInt  = toInteger <$> (digitToInt <$> digit)

  digits :: Parser Char [Char]
  digits = some digit

  intParser :: Parser Char Integer
  intParser = (foldl (\acc x -> acc * 10 + x ) 0) <$> (some digitInt)