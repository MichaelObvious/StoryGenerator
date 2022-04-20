module Main where


import System.Environment
import           DataFileParser
import           Parser
import           System.IO.Unsafe
import           StoryWriter

dataFile :: String
dataFile = "data.txt"

printStory :: FilePath -> IO ()
printStory path = putStrLn $ gen_story $ unsafePerformIO (parse_file path data_parser) 

main :: IO ()
main = do
    args <- getArgs
    if length args > 0 then
        printStory $ head args
    else
        printStory dataFile