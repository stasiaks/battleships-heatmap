{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

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
import Data.Grid (Grid (toVector), cell, Coord)
import Data.Vector (imap)
import Data.Text (Text, pack)
import Data.Decimal (roundTo)

data State =
    State Board [PossibleBoard]

data Event
    = Closed
    | Print
    | Clicked Int
  
fieldLabel :: Field -> Text
fieldLabel Hit = "O"
fieldLabel Miss = "X"
fieldLabel (Chance p) = pack $ show (roundTo 2 (100 * p)) ++ "%"

toGridChild :: Int -> Field -> GridChild Event
toGridChild index field = GridChild { properties = defaultGridChildProperties { leftAttach = (\[x,_] -> fromIntegral x) $ indexToCoord index
                                                                              , topAttach = (\[_,y] -> fromIntegral y) $ indexToCoord index                                                                              }
                                    , child = widget Button [ #label := fieldLabel field
                                                            , #vexpand := True
                                                            , #hexpand := True
                                                            , on #clicked (Clicked index)
                                                            ] }

view' :: State -> AppView Window Event
view' (State board _) =
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
update' (State board possible) Print = Transition (State board possible) (print board >> return Nothing)
update' (State board possible) (Clicked index) = Transition (State board possible) (print (indexToCoord index) >> return Nothing)

main :: IO ()
main = void $ run App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = State (calculateChances freshBoard allBoards) allBoards 
    }

