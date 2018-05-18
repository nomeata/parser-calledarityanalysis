{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PrettyPrint where

-- import Data.Char
-- import Data.List
-- import Data.Typeable
-- import qualified Text.PrettyPrint.Leijen.Text as P hiding ((<$>), char, space)
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Render.Text as P

import Generics.Eot
import Data.Text.Lazy (Text)

-- From: https://generics-eot.readthedocs.io/en/latest/tutorial.html
class EotPrettyPrint eot where
  eotPrettyPrint :: eot -> P.Doc ()

instance (EotPrettyPrint this, EotPrettyPrint next) => EotPrettyPrint (Either this next) where
  eotPrettyPrint (Left fields) = eotPrettyPrint fields
  eotPrettyPrint (Right next) = eotPrettyPrint next

instance EotPrettyPrint Void where
  eotPrettyPrint void = seq void $ error "impossible"

instance (PrettyPrint x, EotPrettyPrint xs) => EotPrettyPrint (x, xs) where
  eotPrettyPrint (x, xs) = prettyPrint x P.<> eotPrettyPrint xs

instance EotPrettyPrint () where
  eotPrettyPrint () = P.emptyDoc

class PrettyPrint a where
  prettyPrint :: a -> P.Doc ()
  default prettyPrint :: (HasEot a, EotPrettyPrint (Eot a)) => a -> P.Doc ()
  prettyPrint = genericPrettyPrint

genericPrettyPrint :: (HasEot a, EotPrettyPrint (Eot a)) => a -> P.Doc ()
genericPrettyPrint = eotPrettyPrint . toEot

instance PrettyPrint Char where
  prettyPrint x = P.pretty x

instance PrettyPrint Text where
  prettyPrint x = P.pretty x

instance PrettyPrint a => PrettyPrint (Maybe a) where
  prettyPrint (Just x) = prettyPrint x
  prettyPrint Nothing = P.emptyDoc

instance PrettyPrint a => PrettyPrint [a] where
  prettyPrint lst = P.hcat $ map prettyPrint lst

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
  prettyPrint (x, y) = (prettyPrint x) P.<> (prettyPrint y)

instance (PrettyPrint a, PrettyPrint b, PrettyPrint c) => PrettyPrint (a, b, c) where
  prettyPrint (x, y, z) = (prettyPrint x) P.<> (prettyPrint y) P.<> (prettyPrint z)

instance (PrettyPrint a, PrettyPrint b, PrettyPrint c, PrettyPrint d) => PrettyPrint (a, b, c, d) where
  prettyPrint (w, x, y, z) = (prettyPrint w) P.<> (prettyPrint x) P.<> (prettyPrint y) P.<> (prettyPrint z)

instance (PrettyPrint a, PrettyPrint b, PrettyPrint c, PrettyPrint d, PrettyPrint e) => PrettyPrint (a, b, c, d, e) where
  prettyPrint (v, w, x, y, z) = (prettyPrint v) P.<> (prettyPrint w) P.<> (prettyPrint x) P.<> (prettyPrint y) P.<> (prettyPrint z)