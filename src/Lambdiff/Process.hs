module Lambdiff.Process where

import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Lambdiff.DiffParse (ParseFileResult(..),
                           ParseChunk(..),
                           ParseLine(..))
import Lambdiff.Types
import Debug.Trace (trace)

isNotNull = (/= "/dev/null")

processParseFile :: ParseFileResult -> FileDiff
processParseFile (ParseFileResult name1 name2 chunks) =
    FileDiff unifiedFilename direction sections
  where
    unifiedFilename = if name1exists then name1 else name2

    name1exists = isNotNull name1
    name2exists = isNotNull name2
    direction = getDirection name1exists name2exists

    getDirection True True = DirectionChanged
    getDirection False True = DirectionAdded
    getDirection True False = DirectionRemoved

    sections = map processChunk chunks

processChunk :: ParseChunk -> DiffSection
processChunk (ParseChunk start1 start2 lines) =
    DiffSection plines
  where
    !plines = (reverse . fst4) $! foldl' processLine 
                ([], start1, start2, Nothing) lines

fst4 (a, b, c, d) = a


processLine :: ([DiffLine], Int, Int, Maybe Int) 
            -> ParseLine 
            -> ([DiffLine], Int, Int, Maybe Int) 
processLine inp (ParseLineComment s) = inp

processLine (all, l1, l2, _) (ParseLineNone s) = 
    (newline : all, l1 + 1, l2 + 1, Nothing)
  where
    newline = DiffLine DirectionNone (Just (l1, s)) (Just (l2, s))

processLine (all, l1, l2, msub) (ParseLineSub s) =
    (newline : all, l1 + 1, l2, Just $ (fromMaybe 0 msub) + 1)
  where
    newline = DiffLine DirectionRemoved (Just (l1, s)) Nothing


-- change case, merge:
processLine (all, l1, l2, Just 1) (ParseLineAdd s) =
    (fixed, l1, l2 + 1, Nothing)
  where
    fixed = mergeChange allsimple
    allsimple = simple : all
    simple = DiffLine DirectionAdded Nothing (Just (l2, s))

-- non-change case:
processLine (all, l1, l2, msub) (ParseLineAdd s) =
    (newline : all, l1, l2 + 1, fmap (subtract 1) msub)
  where
    newline = DiffLine DirectionAdded Nothing (Just (l2, s))


mergeChange :: [DiffLine] -> [DiffLine]
mergeChange lines = 
    mergedLines ++ remainingLines
  where
    (opLines, remainingLines) = span isAddOrSub lines

    isAddOrSub (DiffLine DirectionAdded _ _) = True
    isAddOrSub (DiffLine DirectionRemoved _ _) = True
    isAddOrSub _ = False

    mergedLines = uncurry (zipWith mergeLine) $ splitAt (length opLines `div` 2) opLines

    mergeLine (DiffLine DirectionAdded Nothing add)
              (DiffLine DirectionRemoved sub Nothing) =
                    DiffLine DirectionChanged sub add
