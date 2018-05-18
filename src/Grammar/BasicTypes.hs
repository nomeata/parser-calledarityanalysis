{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module BasicTypes
  ( module BasicTypes
  , Parser
  ) where

import Data.Data
import qualified Data.Text.Lazy as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Pos as M
import Data.Void (Void)

#if MIN_VERSION_megaparsec(6,0,0)
type Parser = M.Parsec Void T.Text -- String is the type of the error message
#else
import Text.Megaparsec.Text (Parser)
#endif

-- VHDL versions this parser supports
data Version = VHDL1993 deriving (Eq, Show, Data)

-- Each token is recorded with it's original position
type TextToken = (M.SourcePos, T.Text)

--
-- Comment
--
newtype Comment = Comment (TextToken, TextToken) deriving (Eq, Show, Data) -- Text = the comment, Space is on next line

--
-- Terminal
--
data T = Ter { t_text :: TextToken
             , t_space :: TextToken -- this is whitespace following this token
                                    -- todo: the source position is nonsense when the text is ""
                                    --       turn this into a Maybe TextToken
             , t_comments :: [Comment] 
             , t_version :: [Version]
             } deriving (Eq, Show, Data)

--
-- NonTerminal
--
data NT a = NTer { nt_token :: a
                 , nt_version :: [Version]
                 } deriving (Eq, Show, Data)