-- | Specialized versions of the general functions in
-- Text.PrettyPrint.HughesPJ.  Normally you shouldn't need these, but
-- they might help if ambiguities arise.
module Text.PrettyPrint.String (

        -- * The document type
        Doc, TextDetails(..),

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ptext, sizedText, zeroWidthText,
        int, integer, float, double, rational,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, doubleQuotes,
        maybeParens, maybeBrackets, maybeBraces, maybeQuotes, maybeDoubleQuotes,

        -- ** Combining documents
        empty,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        nest,
        hang, punctuate,

        -- * Predicates on documents
        isEmpty,

        -- * Utility functions for documents
        first, reduceDoc,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,
        Mode(..),

        -- ** General rendering
        fullRender,

    ) where

import Text.PrettyPrint.HughesPJ hiding
    (text, ptext, sizedText, zeroWidthText, render, renderStyle)

import qualified Text.PrettyPrint.HughesPJ as LL

-- | Specialized version of 'LL.text'
text :: String -> Doc
text = LL.text
{-# INLINE text #-}

-- | Specialized version of 'LL.ptext'
ptext :: String -> Doc
ptext = LL.ptext
{-# INLINE ptext #-}

-- | Specialized version of 'LL.sizedText'
sizedText :: Int -> String -> Doc
sizedText = LL.sizedText
{-# INLINE sizedText #-}

-- | Specialized version of 'LL.zeroWidthText'
zeroWidthText :: Int -> String -> Doc
zeroWidthText = LL.sizedText
{-# INLINE zeroWidthText #-}

-- | Specialized version of 'LL.render'
render :: Doc -> String
render = LL.render
{-# INLINE render #-}

-- | Specialized version of 'LL.renderStyle'
renderStyle :: Style -> Doc -> String
renderStyle = LL.renderStyle
{-# INLINE renderStyle #-}
