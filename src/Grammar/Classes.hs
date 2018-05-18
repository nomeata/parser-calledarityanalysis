{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

--
-- Generic programming with TypeClasses
--

module Classes where

import BasicTypes
import Data.Maybe
import qualified Data.HashSet as HS
import qualified Data.Text.Lazy as T
import Data.String (IsString)

class Rule f a where
  get :: Decorator f => f a

class Monad f => Decorator f where
  n :: [Version] -> f a -> f (NT a) -- n stands for both NT as well as Node (in grammar tree)
  ter :: f T.Text -> [Version] -> f T -- token OPTIONALLY followed by spaces
  ters :: f T.Text -> [Version] -> f T -- token MUST be followed by spaces
  chr :: Char -> [Version] -> f T
  txt :: T.Text -> [Version] -> f T -- token OPTIONALLY followed by spaces
  txts :: T.Text -> [Version] -> f T -- token MUST be followed by spaces
  chrt :: Char -> f Char
  txtt :: String -> f T.Text
  str :: String -> f String
  txtti :: String -> f T.Text -- case insensitive

  -- ostr :: String -> f String -- optional String, doesn't fail, returns "" instead
  -- ostr x = estr (str x)
  -- parser that returns an empty string instead of failing
  estr :: f String -> f String
  estr x = fmap (fromMaybe "") (o x)

  -- combinators
  s :: f a -> f [a]
  m :: f a -> f [a]
  e :: HS.HashSet Char -> f Char -- e stands for element
  c :: [f a] -> f a -- c stands for choose
  o :: f a -> f (Maybe a) -- o stands for optional
  try :: f a -> f a
  -- debug
  dbg :: (Show a) => String -> f a -> f a
  trace :: String -> f a -> f a

  -- helper functions in the grammar
  n93 :: Rule f a => f (NT a)
  n93 = n [VHDL1993] get

  postponed :: f (Maybe T)
  postponed = o $ txt "postponed" [VHDL1993]

  semicolon :: f T
  semicolon = chr ';' [VHDL1993]

  colon :: f T
  colon = chr ':' [VHDL1993]

  parenOpen :: f T
  parenOpen = chr '(' [VHDL1993]

  parenClose :: f T
  parenClose = chr ')' [VHDL1993]

  comma :: f T
  comma = chr ',' [VHDL1993]

  moreComma :: Rule f a => f [(T, NT a)]
  moreComma = m $ do
    cc <- comma
    cont <- n93
    return (cc, cont)
