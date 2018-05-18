{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Decorator where

import Classes

import Basic
import BasicTypes
import CharacterSets
import Control.Monad (when, void)
import Control.Applicative
import Data.Char (toLower, toUpper)
import Data.Foldable
import Data.List
import qualified Data.HashSet as HS
import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Pos as M
import Test.QuickCheck
import Utils

--
-- Generator
--
instance Decorator Gen where
  n version get = (\x -> NTer x version) <$> get
  ter = arbitraryT listOf -- Terminal OPTIONALLY followed by space
  ters = arbitraryT listOf1 -- Terminal MUST be followed by space
  chr char = arbitraryT listOf (pure (T.singleton char))
  txt = atext
  txts = atexts
  chrt = pure
  txtt str = T.pack <$> ci_gen str
  txtti str = T.pack <$> pure str
  str = ci_gen
  s = listOf1
  m = listOf
  e hs = elements (HS.toList hs) 
  c = oneof
  o x = oneof [Just <$> x, pure Nothing]
  try = id
  dbg _ x = x
  trace _ b = b

ci_gen :: String -> Gen String
ci_gen str = make_gen str
  where make_gen [] = pure ""
        make_gen (x:xs) = do
                          x'  <- elements [toLower x, toUpper x]
                          xs' <- make_gen xs
                          return (x':xs')


arbitraryT :: (Gen Char -> Gen String) -> Gen T.Text -> [Version] -> Gen T
arbitraryT quantifier txt_gen version = do
  pos <- arbitrary
  a <- txt_gen
  s <- space_gen quantifier
  c <- listOf arbitrary
  return Ter {t_text=(pos, a), t_space=s, t_comments=c, t_version=version}


-- Generate a text terminal from String (case insensitive)
atext :: T.Text -> [Version] -> Gen T -- Terminal OPTIONALLY followed by space
atext text = arbitraryT listOf (T.pack <$> ci_gen (T.unpack text))

atexts :: T.Text -> [Version] -> Gen T -- Terminal MUST be followed by space
atexts text = arbitraryT listOf1 (T.pack <$> ci_gen (T.unpack text))

--
-- Parser
--
instance Decorator Parser where
  n version get = fmap (\t -> NTer {nt_token=t, nt_version=version}) get
  ter = terminal many
  ters = terminal some
  -- terWS = terminalWS
  -- term ws ver = terminal ws ver
  chr cc = (terminal many) $ T.singleton <$> (M.try $ M.char cc)
  txt t versions = ter (T.pack <$> M.string' (T.unpack t)) versions
  txts t versions = ters (T.pack <$> M.string' (T.unpack t)) versions
  chrt = M.char
  txtt s = T.pack <$> M.string' s
  txtti s = T.pack <$> M.string s
  str = M.string'
  s = (some <$> try)
  m = many
  e hs = M.satisfy (`HS.member` hs)
  c = Data.Foldable.asum
  o x = optional $ M.try x
  try = M.try
  dbg _ x = x
  trace _ b = b


terminal :: (Parser Char -> Parser String) -> Parser T.Text -> [Version] -> Parser T
terminal quantifier text_parser versions = do
  pos <- M.getPosition
  a <- text_parser
  ss@(_, ss_t) <- wp $ T.pack <$> quantifier M.spaceChar
  cc <- many comment
  -- if there was no white space AND if there was no comment AND if the last character was not a delimiter itself
  -- then the lookahead should match on a special character or end of file
  when (T.length ss_t == 0 && Data.List.length cc == 0 && not (HS.member (T.last a) delimitersNl) ) (M.lookAhead $ c [void $ e delimitersNl, M.eof])
  return Ter {t_text=(pos, a), t_space=ss, t_comments=cc, t_version=versions}
