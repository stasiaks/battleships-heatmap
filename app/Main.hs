{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (Main.main) where

import Lib
import Control.Monad (void, liftM)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Gtk ( Window(..)
              , Grid (..)
              , Button (..), buttonClicked
              )
import GI.Gtk.Declarative.Container.Grid
import Data.Grid (toNestedLists, Grid (toVector))
import Data.Vector (fromList, imap)
import Data.Text (Text, pack)

newtype State =
    State Board

data Event
    = Closed
    | Print
  
fieldLabel :: Field -> Text
fieldLabel Hit = "O"
fieldLabel Miss = "X"
fieldLabel (Chance p) = pack $ show p ++ "%"

toGridChild :: Int -> Field -> GridChild Event
toGridChild index field = GridChild { properties = defaultGridChildProperties { leftAttach = fromIntegral $ index `mod` 10
                                                                              , topAttach = fromIntegral $ index `div` 10
                                                                              }
                                    , child = widget Button [ #label := fieldLabel field
                                                            , on #clicked Print 
                                                            ] }

view' :: State -> AppView Window Event
view' (State board) =
    bin
        Window
        [ #title := "Battleships"
        , on #deleteEvent (const (True, Closed))
        , #widthRequest := 400
        , #heightRequest := 300 
        ]
        $ container 
            Grid 
            [ #rowSpacing := 4, #columnSpacing := 4, #margin := 4 ]
            $ imap toGridChild (toVector board) 

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit
update' (State board) Print = Transition (State board) (print board >> return Nothing)

main :: IO ()
main = void $ run App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = State freshBoard 
    }

