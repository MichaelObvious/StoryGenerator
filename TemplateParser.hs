module TemplateParser where

import Control.Applicative

import Parser

data ParsedValue
    = Text String
    | Noun
    | Adjective
    | Verb
    deriving (Show, Eq)

template_tag :: Parser ParsedValue
template_tag = char_parser '@' *> (f <$> stringliteral) <* char_parser '@'
    where stringliteral = (string_parser "noun" <|> string_parser "adjective" <|> string_parser "verb")
          f "noun"      = Noun
          f "adjective" = Adjective
          f "verb"      = Verb
          -- should never happen
          f _           = undefined

template_text :: Parser ParsedValue
template_text = Text <$> not_null(span_parser (\c -> c /= '@'))

template_parser :: Parser [ParsedValue]
template_parser = many (template_text <|> template_tag) <|> pure []
