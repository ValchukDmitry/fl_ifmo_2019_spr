module Tokenizer where
import qualified Control.Applicative as CA
import Data.Char (isDigit, digitToInt, isLetter)

data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (String,a) }

instance Functor Parser where
    fmap g (Parser p) = Parser $ (fmap . fmap . fmap) g p

instance Applicative Parser where
    pure x = Parser $ \s -> Just (s, x)
    Parser u <*> Parser v = Parser f where
        f xs = case u xs of
            Nothing -> Nothing
            Just (xs', g) -> case v xs' of
                Nothing -> Nothing
                Just (xs'', x) -> Just (xs'', g x)


instance CA.Alternative Parser where
    empty = Parser $ \_ -> Nothing
    Parser u <|> Parser v = Parser f where
        f xs = case u xs of
            Nothing -> v xs
            z -> z

tokenize :: String -> [Token]
tokenize input = tokenize_word <$> words input where
    tokenize_word word = let res = runParser resultParser word in
        case res of
            Nothing -> undefined
            Just (_, t) -> t

digitsParser = Parser digitsP' where
    digitsP' (c:cs) =
        let res = digitsP (c:cs) in
            case res of
                Nothing -> Nothing
                Just (s, x) -> Just (s, Number $ x)
    digitsP [c] = if isDigit c then Just ("", digitToInt c) else Nothing
    digitsP (c:cs) = if isDigit c then
        let res = digitsP cs in
        case res of
            Nothing -> Nothing
            Just (s, x) -> Just (s, (digitToInt c * 10 ^ length cs) + x)
         else Nothing

keyWordParser = Parser $ \w -> if elem w keyWords then Just ("", KeyWord w) else Nothing where
    keyWords =
        ["and", "del", "for", "is", "raise", "assert", "elif", "from", "lambda",
        "return", "break", "else", "global", "not", "try", "class", "except",
        "if", "or", "while", "continue", "exec", "import", "pass", "def",
        "finally", "in", "print", "yield"]

identParser = Parser identP where
    identP (c:cs) | isLetter c || c== '_' = let res = runParser identP' (c:cs) in
        case res of
            Nothing -> Nothing
            Just (s, x) -> Just (s, Ident x)
        where
            identP' = (:) <$> (Parser identTail) <*> identP' CA.<|> pure "" where
                identTail [c] = Just ("", c)
                identTail (c:cs) | isLetter c || c== '_' || isDigit c = Just (cs, c)
                identTail _ = Nothing
    identP _ = Nothing

resultParser = keyWordParser CA.<|> digitsParser CA.<|> identParser