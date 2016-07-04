{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.PrettyPrint.ListLike (
        UString, AString
    ) where

import Control.DeepSeq ( NFData )
import qualified Data.ListLike as LL
import Data.ListLike.UTF8 ()
import Data.String (IsString)
import Data.Text as Strict (Text)
import Data.Text.Lazy as Lazy (Text)
import Data.Text.Lazy.Builder (Builder)

class (Eq string,
       IsString string,
       LL.ListLike string Char,
       LL.ListLikeIO string Char,
       LL.StringLike string,
       NFData string) =>
    UString string

instance UString String
instance UString Builder

-- | AString is an instance of UString to be used for internal
-- operations when we need to disambiguate an expression.
-- type AString = String -- -- UnitLargeDoc takes 5 to 6 seconds
type AString = Builder

instance UString Strict.Text
instance UString Lazy.Text
