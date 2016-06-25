{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.PrettyPrint.ListLike (
        UString, AString
    ) where

import Control.DeepSeq ( NFData(rnf) )
import Data.ByteString (ByteString)
import qualified Data.ListLike as LL
import Data.ListLike.UTF8 ()
import Data.String (IsString(fromString))
import "utf8-string" Data.String.UTF8 as UTF8 (fromRep, toRep, UTF8)
import Data.Text as Strict (Text)
import Data.Text.Lazy as Lazy (Text)

class (Eq string,
       IsString string,
       LL.ListLike string Char,
       LL.ListLikeIO string Char,
       LL.StringLike string,
       NFData string) =>
    UString string

instance UString String

-- | AString is an instance of UString to be used for internal
-- operations when we need to disambiguate an expression.
type AString = String -- -- UnitLargeDoc takes 5 to 6 seconds
-- type AString = UTF8 ByteString -- 21.7 s., +RTS -K10000000 -RTS
-- type AString = Strict.Text
-- type AString = Lazy.Text -- UnitLargeDoc takes 45 to 55 seconds and kills machine

instance UString Strict.Text
instance UString Lazy.Text

instance NFData (UTF8 ByteString) where
    rnf = rnf . UTF8.toRep
instance IsString (UTF8 ByteString) where
    fromString = UTF8.fromRep . fromString
instance UString (UTF8 ByteString)
