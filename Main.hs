module Main where

data ParsedValue
    = Text String
    | Noun
    | Adjective
    | Verb
    deriving (Show, Eq)

newtype Parser a = Parser { run_parser :: String -> Maybe (String, a) }

char_parser :: Char -> Parser Char
char_parser c = Parser $ \input ->
    case input of
        x:xs | x == c -> Just (xs, c)
        _             -> Nothing

main :: IO ()
main = putStr "yo"
