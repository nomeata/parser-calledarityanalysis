{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Basic where

import BasicTypes
import CharacterSets
import Control.Lens.TH
import Control.Applicative
import PrettyPrint
import qualified Data.Text.Lazy as T
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Render.Text as P
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Pos as M
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Utils

-- low level primitive to help out creating terminals
-- wp stands for: with position
wp :: Parser a -> Parser (M.SourcePos, a)
wp parser = do
  pos <- M.getPosition
  a <- parser
  return (pos, a)

--
-- Comment
--
instance Arbitrary Comment where
  arbitrary = do
    pos_comment <- arbitrary
    -- "--" not included in ast
    txt <- fmap (T.filter (`notElem` ("\r\n"::String))) arbitrary -- whitespace until the end of the line is regarded to be a part of the comment
    end_of_line <- elements ["\r\n", "\n"]
    space <- fmap (\(p, t) -> (p, T.concat [end_of_line, t])) $ space_gen listOf -- possibly more whitespace or empty lines after the newline
    return $ Comment ((pos_comment, txt), space)
  shrink (Comment (a, b)) = [Comment (x, y) | (x, y) <- shrink (a, b)]
instance P.Pretty Comment where -- possibly could make this an instance of PrettyPrint .. but it doesn't matter much
  pretty (Comment ((_, c), (_, s))) = (P.pretty ("--" :: T.Text) ) P.<> (P.pretty c) P.<> (P.pretty s)
comment :: Parser Comment
comment = do
  c <- wp $ M.string "--" *> (T.pack <$> M.manyTill M.anyChar (M.lookAhead M.eol))
  space <- wp $ T.pack <$> many M.spaceChar -- comment line is optionally followed by a newline and more optional whitespace/newlines
  return $ Comment (c, space)
makePrisms ''Comment

--
-- Terminal
--
instance Arbitrary T where
  arbitrary = undefined
  shrink (Ter tt space@(space_pos, space_text) _ version)
    | T.length space_text == 0 = [Ter tt space             [] version]
    | otherwise                = [Ter tt (space_pos, "\n") [] version]
instance PrettyPrint T where
  prettyPrint Ter {t_text=(_, t), t_space=(_, s), t_comments=c} =
    (P.pretty t) P.<> (P.pretty s) P.<> (P.hcat $ map P.pretty c)

--
-- NonTerminal
--
instance Arbitrary a => Arbitrary (NT a) where
  arbitrary = undefined
  shrink (NTer x version) = fmap (\y -> NTer y version) (shrink x)
instance PrettyPrint a => PrettyPrint (NT a) where
  prettyPrint (NTer tok _) = prettyPrint tok