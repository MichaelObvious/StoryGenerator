{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
module Main where

import           Control.Monad                 (void)
import           Data.Text                     (Text, pack, unpack)
import           DataFileParser
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Parser
import           System.IO.Unsafe
import           StoryWriter

data State = State { story_text :: Text, data_file :: Text }

data Event = Closed
          | DataFileChanged Text
          | GenerateStory

view' :: State -> AppView Gtk.Window Event
view' s = bin Gtk.Window [ #title := "MichaelObvious's Funny Story Generator", #widthRequest := 500, #heightRequest := 250, on #deleteEvent (const (True, Closed)) ]
    (container Gtk.Box [ #orientation := Gtk.OrientationVertical ] [ story_label, datafile_form ])
    where story_label = BoxChild defaultBoxChildProperties { expand = True, fill = True }
              $ widget Gtk.Label [ #label := story_text s, #halign := Gtk.AlignCenter, #valign := Gtk.AlignStart, #wrap := True ] 
          datafile_form = container Gtk.Box
              [ #orientation := Gtk.OrientationHorizontal ] [ datafile_input, generate_button ]
          datafile_input = widget Gtk.Entry
              [ #text := data_file s, #placeholderText := "data file...", onM #changed (fmap DataFileChanged . Gtk.entryGetText) ]
          generate_button = widget Gtk.Button
              [ #label := "Generate", on #pressed GenerateStory ]

update' :: State -> Event -> Transition State Event
update' s e = case e of
    DataFileChanged t -> Transition s { data_file = t } (pure Nothing)
    GenerateStory     -> Transition s { story_text = pack $ gen_story $ unsafePerformIO (parse_file (unpack $ data_file s) data_parser) } (pure Nothing)
    Closed            -> Exit

{-run_story :: IO (String)
run_story = do
  parsed_data <- 
  return (gen_story parsed_data)-}

main :: IO ()
main = void $ run App { view = view', update = update', inputs = [], initialState = State { story_text = "", data_file = "data.txt"} }
{-main = do
    story <- run
    putStrLn $ "\n========== RANDOMLY GENERATED STORY ==========\n" ++ story ++ "\n"-}