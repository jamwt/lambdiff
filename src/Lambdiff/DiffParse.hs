module Lambdiff.DiffParse where

import Data.ByteString.Char8 as S
import Control.Applicative ((<|>))

import qualified Data.Attoparsec as Atto
import qualified Data.Attoparsec.Combinator as AttoComb
import qualified Data.Attoparsec.Char8 as AttoC

data ParseFileResult = ParseFileResult S.ByteString S.ByteString [ParseChunk]
  deriving Show

data ParseChunk = ParseChunk Int Int [ParseLine]
  deriving Show

data ParseLine = ParseLineNone S.ByteString
               | ParseLineAdd S.ByteString
               | ParseLineSub S.ByteString
               | ParseLineComment S.ByteString
  deriving Show

diffParser :: Atto.Parser [ParseFileResult]
diffParser = AttoComb.many' parseDiff

parseDiff :: Atto.Parser ParseFileResult
parseDiff = do
    (f1, f2) <- consumeHeader
    chunks <- Atto.many' parseChunk
    return $ ParseFileResult f1 f2 chunks

parseChunk :: Atto.Parser ParseChunk
parseChunk = do
    Atto.string "@@ -"
    lineNo1 <- AttoC.decimal
    _ <- Atto.many1 $ AttoC.notChar '+'
    _ <- AttoC.char '+'
    lineNo2 <- AttoC.decimal
    _ <- Atto.many1 $ AttoC.notChar '\n'
    _ <- AttoC.char '\n'
    lines <- Atto.many1 parseChunkLine
    return $ ParseChunk lineNo1 lineNo2 lines

parseChunkLine :: Atto.Parser ParseLine
parseChunkLine = do
    const <- noLead <|> addLead <|> subLead <|> comLead
    restOfLine <- AttoC.takeWhile (/='\n')
    AttoC.char '\n'
    return $ const $ S.concat [restOfLine, "\n"]

noLead = AttoC.char ' ' >> (return ParseLineNone)
addLead = AttoC.char '+' >> (return ParseLineAdd)
subLead = AttoC.char '-' >> (return ParseLineSub)
comLead = AttoC.char '\\' >> (return ParseLineComment)

-- | Handle both git-style unified diffs and
-- | diff -u style ones
consumeHeader :: Atto.Parser (S.ByteString, S.ByteString)
consumeHeader = do
    (Atto.string "diff " >> AttoC.scan "" findFileTop) <|> (Atto.string "---")
    _  <- AttoC.char ' '
    f1 <- AttoC.takeWhile1 (\c -> and [(c/='\n'), (c/='\t')])
    _  <- AttoC.takeWhile (/='\n')
    _  <- Atto.string "\n+++ "
    f2 <- AttoC.takeWhile1 (\c -> and [(c/='\n'), (c/='\t')])
    _  <- AttoC.takeWhile (/='\n')
    _  <- AttoC.char '\n'
    return (f1, f2)
  where
    findFileTop "---\n" ' '  = Nothing
    findFileTop _       '\n' = Just "\n"
    findFileTop s       '-'  = Just $ '-' : s
    findFileTop _        _   = Just ""


