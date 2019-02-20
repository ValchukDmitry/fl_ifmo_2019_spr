module Tokenizer where

import Combinators
import Prelude hiding (fmap)

import Data.Char

data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

satisfy :: (token -> Bool) -> Parser [token] token
satisfy pr = Parser f
    where
    f (c:cs) | pr c  = Just (cs,c)
    f _              = Nothing


token :: Eq token => token -> Parser [token] token
token t = satisfy (==t)

eof :: Parser [token] ()
eof = Parser $ \s -> case s of
                          [] -> Just ([], ())
                          _  -> Nothing

tokenize :: String -> [Token]
tokenize input = case runParser (many finalParser <* eof) input of
    Just (_, tokens) -> tokens
    _ -> []

finalParser = ((KeyWord <$> parseKeyWord) <|>
            (Number <$> parseNumber) <|>
            (Ident <$> parseIdent)) <* end
                where
                    end = unt (some $ token ' ') eof where
                        unt a b = (const () <$> a) <|> (const () <$> b)

parseKeyWord :: Parser String String
keyWords =
    ["and", "del", "for", "is", "raise", "assert", "elif", "from", "lambda",
    "return", "break", "else", "global", "not", "try", "class", "except",
    "if", "or", "while", "continue", "exec", "import", "pass", "def",
    "finally", "in", "print", "yield"]
parseKeyWord = keywords keyWords


parseIdent :: Parser String String
parseIdent = do
    x <- (satisfy isLetter) <|> (token '_')
    xs <- many ((satisfy isLetter) <|> (token '_') <|> (satisfy isDigit))
    return (x:xs)

parseNumber :: Parser String Int
parseNumber = fmap (read) $
    keywords ["0"] <|>
    do
        let nonZero = foldr (<|>) Combinators.fail (token <$> "123456789")
        x <- nonZero
        xs <- many $ satisfy isDigit
        return (x:xs)
