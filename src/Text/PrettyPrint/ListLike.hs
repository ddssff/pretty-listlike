{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.PrettyPrint.ListLike (
        UString, AString
    ) where

import Control.DeepSeq ( NFData )
import qualified Data.ListLike as LL
import Data.String (IsString)

class (Eq string,
       IsString string,
       LL.ListLike string Char,
       LL.ListLikeIO string Char,
       LL.StringLike string,
       NFData string) =>
    UString string

instance UString String

-- | An instance of UString to be used for internal operations where
-- we need to disambiguate an expression.
type AString = String
