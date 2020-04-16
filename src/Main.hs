{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import           DataFileParser
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Parser
import           StoryWriter

type State = ()

data Event = Closed

view' :: State -> AppView Gtk.Window Event
view' _ = bin Gtk.Window [#title := "MichaelObvious's Funny Story Generator", on #deleteEvent (const (True, Closed))]
    $ widget Gtk.Label [#label := "yo"]

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit

{-run_story :: IO (String)
run_story = do
  parsed_data <- parse_file "data.txt" data_parser
  return (gen_story parsed_data)-}

main :: IO ()
main = run App { view = view', update = update', inputs = [], initialState = () }
{-main = do
    story <- run
    putStrLn $ "\n========== RANDOMLY GENERATED STORY ==========\n" ++ story ++ "\n"-}