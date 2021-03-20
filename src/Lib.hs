{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}

module Lib
    ( Field (..)
    , Board
    , PossibleField
    , PossibleBoard
    , indexToCoord
    , freshBoard
    , allBoards
    , calculateChances
    ) where

import Data.Decimal
import qualified Data.Vector as V
import GHC.Base
import Data.Grid

data Field = Hit | Miss | Chance Decimal
    deriving (Show)

type Board = Grid [10,10] Field

data PossibleField = Ship | Water
type PossibleBoard = Grid [10,10] PossibleField

indexToCoord :: Int -> Coord [10, 10]
indexToCoord index = [index `mod` 10, index `div` 10]

freshBoard :: Board
freshBoard = generate $ \i-> Chance 0

allBoards :: [PossibleBoard]
allBoards = [generate (const Ship) | x <-[1..3]]


occurences :: [PossibleBoard] -> Grid [10, 10] Integer
occurences = foldl (liftA2 (\a f -> a + case f of
                                    Ship -> 1
                                    Water -> 0)) $ generate (const 0)

calculateChances :: Board -> [PossibleBoard] -> Board
calculateChances board [] = board
calculateChances board pbs = liftA2 (\a b -> case a of
                                        Hit -> Hit
                                        Miss -> Miss
                                        Chance _ -> Chance $ fromIntegral b / fromIntegral possibilites) board $ occurences pbs
    where possibilites = length pbs
