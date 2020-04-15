module DataFileParser where

import Control.Applicative

import Parser

data ConfigData
    = Adjectives [String]
    | Nouns      [String]
    | Verbs      [String]
    | Adverbs    [String]
    | Templates  [String]
    deriving (Show, Eq)

split_on :: (Char -> Bool) -> String -> [String]
split_on p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split_on p s''
                            where (w, s'') = break p s'

data_field :: Parser ConfigData
data_field = char_parser '|' *> (tag_literal <*> text)
    where tag_literal = f <$> (string_parser "ADJECTIVES" <|> string_parser "NOUNS" <|> string_parser "VERBS" <|> string_parser "ADVERBS" <|> string_parser "TEMPLATES")
          text = not_null (span_parser (/= '|'))
          polish = filter (/= '\r')
          discard_null = filter (not . null)
          {-replace c1 c2 c3
                | c1 == c3  = c2
                | otherwise = c3-}
          f tag txt = case tag of "ADJECTIVES" -> Adjectives $ discard_null $ lines $ polish txt
                                  "NOUNS"      -> Nouns      $ discard_null $ lines $ polish txt
                                  "VERBS"      -> Verbs      $ discard_null $ lines $ polish txt
                                  "ADVERBS"    -> Adverbs    $ discard_null $ lines $ polish txt
                                  "TEMPLATES"  -> Templates  $ discard_null $ split_on (== '$') $ polish txt
                                  -- should never happen
                                  _            -> undefined


data_parser :: Parser [ConfigData]
data_parser = many data_field <|> pure []
