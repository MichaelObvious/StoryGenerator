module TemplateParser where

import Control.Applicative

import Parser

data TemplateValue
    = TemplateText String
    | TemplateNoun
    | TemplateProperNoun
    | TemplateAdjective
    | TemplateVerb
    | TemplateAdverb
    | TemplateNumber
    deriving (Show, Eq)

template_tag :: Parser TemplateValue
template_tag = char_parser '@' *> (f <$> tag_literal) <* char_parser '@'
    where tag_literal   = (string_parser "noun" <|> string_parser "adjective" <|> string_parser "verb" <|> string_parser "adverb" <|> string_parser "number" <|> string_parser "propernoun")
          f "noun"       = TemplateNoun
          f "adjective"  = TemplateAdjective
          f "verb"       = TemplateVerb
          f "adverb"     = TemplateAdverb
          f "number"     = TemplateNumber
          f "propernoun" = TemplateProperNoun
          -- should never happen
          f _           = undefined

template_text :: Parser TemplateValue
template_text = TemplateText <$> not_null (span_parser (/= '@'))

template_parser :: Parser [TemplateValue]
template_parser = many (template_text <|> template_tag) <|> pure []
