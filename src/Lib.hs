{-# LANGUAGE DataKinds #-}
module Lib
    ( Field (..)
    , Board
    , freshBoard
    ) where

import Data.Decimal
import GHC.Base
import Data.Grid

data Field = Hit | Miss | Chance Decimal
    deriving (Show)

type Board = Grid [10,10] Field

freshBoard :: Board
freshBoard = generate $ \i-> Chance $ fromIntegral i
