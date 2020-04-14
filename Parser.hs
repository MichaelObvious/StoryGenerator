module Parser where

import Control.Applicative

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

parse_file :: FilePath -> Parser a -> IO (Maybe a)
parse_file filename parser = do
    input <- readFile filename
    return (snd <$> run_parser parser input)
