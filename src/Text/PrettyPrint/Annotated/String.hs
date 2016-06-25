-- | Specialized versions of the general functions in
-- Text.PrettyPrint.Annotated.HughesPJ.  Normally you shouldn't need these, but
-- they might help if ambiguities arise.
module Text.PrettyPrint.Annotated.String (

        -- * The document type
        Doc, TextDetails(..), AnnotDetails(..),

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

        -- ** Annotating documents
        annotate,

        -- * Predicates on documents
        isEmpty,

        -- * Utility functions for documents
        first, reduceDoc,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Annotation rendering
        renderSpans, Span(..),
        renderDecorated,
        renderDecoratedM,

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,
        Mode(..),

        -- ** General rendering
        fullRender,

    ) where

import Text.PrettyPrint.Annotated.HughesPJ hiding
    (text, ptext, sizedText, zeroWidthText,
     render, renderStyle, renderSpans, renderDecorated, renderDecoratedM)

import qualified Text.PrettyPrint.Annotated.HughesPJ as LL

-- | Specialized version of 'LL.text'
text :: String -> Doc a
text = LL.text
{-# INLINE text #-}

-- | Specialized version of 'LL.ptext'
ptext :: String -> Doc a
ptext = LL.ptext
{-# INLINE ptext #-}

-- | Specialized version of 'LL.sizedText'
sizedText :: Int -> String -> Doc a
sizedText = LL.sizedText
{-# INLINE sizedText #-}

-- | Specialized version of 'LL.zeroWidthText'
zeroWidthText :: Int -> String -> Doc a
zeroWidthText = LL.sizedText
{-# INLINE zeroWidthText #-}

-- | Specialized version of 'LL.render'
render :: Doc a -> String
render = LL.render
{-# INLINE render #-}

-- | Specialized version of 'LL.renderStyle'
renderStyle :: Style -> Doc a -> String
renderStyle = LL.renderStyle
{-# INLINE renderStyle #-}

-- | Specialized version of 'LL.renderSpans'
renderSpans :: Doc ann -> (String,[Span ann])
renderSpans = LL.renderSpans
{-# INLINE renderSpans #-}

-- | Specialized version of 'LL.renderDecorated'
renderDecorated :: (ann -> String) -- ^ Starting an annotation.
                -> (ann -> String) -- ^ Ending an annotation.
                -> Doc ann -> String
renderDecorated = LL.renderDecorated
{-# INLINE renderDecorated #-}

-- | Specialized version of 'LL.renderDecoratedM'
renderDecoratedM :: (Monad m)
                 => (ann    -> m r) -- ^ Starting an annotation.
                 -> (ann    -> m r) -- ^ Ending an annotation.
                 -> (String -> m r) -- ^ Text formatting.
                 -> m r             -- ^ Document end.
                 -> Doc ann -> m r
renderDecoratedM = LL.renderDecoratedM
{-# INLINE renderDecoratedM #-}
