module Main where

import DataFileParser
import Parser
import StoryWriter

run :: IO (String)
run = do
  parsed_data <- parse_file "data.txt" data_parser
  return (gen_story parsed_data)

main :: IO ()
main = do
    story <- run
    putStrLn $ "\n========== RANDOMLY GENERATED STORY ==========\n    " ++ story ++ "\n"