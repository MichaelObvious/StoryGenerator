module Main where

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
        (input'', a) <- p2 input
        Just (input'', f a)

char_parser :: Char -> Parser Char
char_parser c = Parser f
    where
        f (x:xs)
            | x == c    = Just (xs, c)
            | otherwise = Nothing
        f [] = Nothing

string_parser :: String -> Parser String
string_parser str = undefined --sequenceA $ map char_parser str

main :: IO ()
main = putStr "yo"
