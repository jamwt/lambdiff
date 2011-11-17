module Lambdiff.Types where

import Data.ByteString.Char8 as S

data FileDiff = FileDiff S.ByteString DiffDirection [DiffSection]
  deriving Show
data DiffDirection = DirectionAdded
                   | DirectionRemoved
                   | DirectionChanged
                   | DirectionNone
  deriving Show

data DiffSection = DiffSection [DiffLine]
  deriving Show

data DiffLine = DiffLine DiffDirection (Maybe LineData) (Maybe LineData)
  deriving Show

type LineData = (Int, S.ByteString)
