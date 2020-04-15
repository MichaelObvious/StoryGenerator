module Main where

import DataFileParser
import Parser
import StoryWriter

gen_story :: Maybe [ConfigData] -> String
gen_story parsed_data = show $ extract_data ([], [], [], [], []) parsed_data

run :: IO (String)
run = do
  parsed_data <- parse_file "data.txt" data_parser
  return (gen_story parsed_data)

main :: IO ()
main = do
    story <- run
    putStrLn story