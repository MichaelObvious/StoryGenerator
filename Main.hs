module Main where

import Control.Applicative

import Parser

data ParsedValue
    = Text String
    | Noun
    | Adjective
    | Verb
    deriving (Show, Eq)

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
