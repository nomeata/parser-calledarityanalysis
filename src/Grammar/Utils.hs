{-# LANGUAGE CPP #-}

module Utils where

import CharacterSets
import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Pos as M
import Test.QuickCheck


emptySourcePos :: M.SourcePos
emptySourcePos = M.SourcePos { M.sourceName = "", M.sourceLine = M.unsafePos 1, M.sourceColumn = M.unsafePos 1}

space_gen :: (Gen Char -> Gen String) -> Gen (M.SourcePos, T.Text)
space_gen list_func = do
  pos <- arbitrary
  space <- fmap T.pack $ list_func $ elements space_chars
  return (pos, space)
