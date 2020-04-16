module DataFileParser where

import Control.Applicative

import Parser

data ConfigData
    = Adjectives  [String]
    | Nouns       [String]
    | ProperNouns [String]
    | Verbs       [String]
    | Adverbs     [String]
    | Templates   [String]
    deriving (Show, Eq)

split_on :: (Char -> Bool) -> String -> [String]
split_on p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split_on p s''
                            where (w, s'') = break p s'

data_field :: Parser ConfigData
data_field = char_parser '|' *> (tag_literal <*> text)
    where tag_literal = f <$> (string_parser "ADJECTIVES" <|> string_parser "NOUNS" <|> string_parser "VERBS" <|> string_parser "ADVERBS" <|> string_parser "TEMPLATES" <|> string_parser "PROPERNOUNS")
          text = not_null (span_parser (/= '|'))
          polish = filter (/= '\r')
          discard_null = filter (not . null)
          f tag txt = case tag of "ADJECTIVES"  -> Adjectives  $ discard_null $ lines $ polish txt
                                  "NOUNS"       -> Nouns       $ discard_null $ lines $ polish txt
                                  "PROPERNOUNS" -> ProperNouns $ discard_null $ lines $ polish txt
                                  "VERBS"       -> Verbs       $ discard_null $ lines $ polish txt
                                  "ADVERBS"     -> Adverbs     $ discard_null $ lines $ polish txt
                                  "TEMPLATES"   -> Templates   $ discard_null $ split_on (== '$') $ polish txt
                                  -- should never happen
                                  _            -> undefined


data_parser :: Parser [ConfigData]
data_parser = many data_field <|> pure []

extract_data :: ([String], [String], [String], [String], [String], [String]) -> Maybe [ConfigData] -> ([String], [String], [String], [String], [String], [String])
extract_data _ Nothing                            = undefined -- oops, the parser has made an oopsie doopsie fuckie wuckie
extract_data (adjs, ns, pns, vs, advs, tmps) (Just xs) = case xs of
    []   -> (adjs, ns, pns, vs, advs, tmps)
    y:ys -> case y of
        Adjectives adjectives -> extract_data (adjs ++ adjectives, ns, pns, vs, advs, tmps) $ Just ys
        Nouns nouns           -> extract_data (adjs, ns ++ nouns, pns, vs, advs, tmps)      $ Just ys
        ProperNouns nouns     -> extract_data (adjs, ns, pns ++ nouns, vs, advs, tmps)      $ Just ys
        Verbs verbs           -> extract_data (adjs, ns, pns, vs ++ verbs, advs, tmps)      $ Just ys
        Adverbs adverbs       -> extract_data (adjs, ns, pns, vs, advs ++ adverbs, tmps)    $ Just ys
        Templates templates   -> extract_data (adjs, ns, pns, vs, advs, tmps ++ templates)  $ Just ys
