module Main where

import Control.Applicative

data ParsedValue
    = Text String
    | Noun
    | Adjective
    | Verb
    deriving (Show, Eq)

newtype Parser a = Parser { run_parser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $
        \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Applicative Parser where
    pure x                      = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (input', f)  <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Alternative Parser where
    empty                       = Parser $ (const Nothing)
    (Parser p1) <|> (Parser p2) = Parser $ \input -> do
        p1 input <|> p2 input

char_parser :: Char -> Parser Char
char_parser c = Parser f
    where
        f (x:xs)
            | x == c    = Just (xs, c)
            | otherwise = Nothing
        f [] = Nothing

string_parser :: String -> Parser String
string_parser = sequenceA . map char_parser

span_parser :: (Char -> Bool) -> Parser String
span_parser f = Parser $ \input ->
    let (token, rest) = span f input
    in Just (rest, token)

not_null :: Parser [a] -> Parser [a]
not_null (Parser p) = Parser $ \input -> do
    (input', xs) <- p input
    if null xs then Nothing else Just (input', xs)

sep_by :: Parser a -> Parser b -> Parser [b]
sep_by sep elem = (:) <$> elem <*> many (sep *> elem)

quote_tag :: Parser ParsedValue
quote_tag = char_parser '@' *> (f <$> stringliteral) <* char_parser '@'
    where stringliteral = (string_parser "noun" <|> string_parser "adjective" <|> string_parser "verb")
          f "noun"      = Noun
          f "adjective" = Adjective
          f "verb"      = Verb
          -- should never happen
          f _           = undefined

quote_text :: Parser ParsedValue
quote_text = Text <$> not_null(span_parser (\c -> c /= '@'))

quote_parser :: Parser [ParsedValue]
quote_parser = many (quote_text <|> quote_tag) <|> pure []

main :: IO ()
main = putStr "yo"
