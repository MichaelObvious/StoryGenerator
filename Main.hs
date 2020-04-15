module Main where

import Parser
import DataFileParser
-- import TemplateParser

main :: IO ()
main = do
    parsed_data <- parse_file "data.txt" data_parser
    putStrLn $ show $ extract_data ([], [], [], [], []) $ parsed_data
