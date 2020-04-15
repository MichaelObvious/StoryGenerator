module StoryWriter where

import System.Random
import System.IO.Unsafe

import DataFileParser
import Parser
import TemplateParser

pick_one :: [a] -> a
pick_one ts = ts !! (unsafePerformIO randomIO `mod` length ts)

gen_story :: Maybe [ConfigData] -> String
gen_story parsed_data = do
    write_story $ to_good $ extract_data ([], [], [], [], [], []) parsed_data
    where unwrap (Just x)  = snd x
          unwrap Nothing = [TemplateText ""]
          to_good (as, ns, pns, vs, ads, ts) = ((as, ns, pns, vs, ads), (unwrap $ pick_one (map (run_parser template_parser) ts), ""))

write_story :: (([String], [String], [String], [String], [String]), ([TemplateValue], String)) -> String
write_story (ws, (t_vals, story)) = case t_vals of
    t:ts -> write_story (ws, (ts, story ++ (to_text ws t)))
    []   -> polish story
    where to_text (adjs, ns, pns, vs, advs) template = case template of
              TemplateAdjective  -> pick_one adjs
              TemplateNoun       -> pick_one ns  
              TemplateProperNoun -> pick_one pns
              TemplateVerb       -> pick_one vs  
              TemplateAdverb     -> pick_one advs
              TemplateNumber     -> show $ (unsafePerformIO randomIO :: Int)
              TemplateText t     -> t
          polish = dropWhile (== '\n')
