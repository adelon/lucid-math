{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Math
    ( module Lucid.Math
    -- * Re-exports from "Lucid.Html5".
    , module Export
    ) where

import Data.Text (Text)

import Lucid.Base
import Lucid.Html5 as Export (a_, height_, href_, rowspan_, width_)
import qualified GHC.TypeLits as variables


-- * The top-level @math@ MathML element

-- | Top-level element for MathML content. All mathematical content must be
-- wrapped by this element. There should not be a nested 'math_' within it.
math_ :: Term arg result => arg -> result
math_ = term "math"

-- | If "block", display the element outside the current span of text as
-- a separate block. Analogous to LaTeX's @\\[..\\]@.
display_ :: Text -> Attributes
display_ = makeAttributes "display"

-- | Displays the element outside the current span of text as a separate block.
-- Analogous to LaTeX's @\\[..\\]@.
displayblock_ :: Attributes
displayblock_ = display_ "block"

-- | Displays the element within the current span of text.
-- Analogous to LaTeX's @\\(..\\)@ and @$...$@.
displayinline_ :: Attributes
displayinline_ = display_ "inline"

-- | Text alternative for assistive technologies.
alttext_ :: Text -> Attributes
alttext_ = makeAttributes "alttext"


-- * Global attributes


-- | This attribute indicates whether formulas should try to minimize the logical height (if 'False') or not (if 'True') e.g. by changing the size of content or the layout of scripts.
displaystyle_ :: Bool -> Attributes
displaystyle_ value = makeAttributes "displaystyle" (if value then "true" else "false")

-- | Presentational hint for the @math-depth@ of an element. This accepts the
-- full MathML syntax, including relative values such as @+1@.
scriptlevel_ :: Text -> Attributes
scriptlevel_ = makeAttributes "scriptlevel"

-- | Math color hint.
mathcolor_ :: Text -> Attributes
mathcolor_ = makeAttributes "mathcolor"

-- | Math background hint.
mathbackground_ :: Text -> Attributes
mathbackground_ = makeAttributes "mathbackground"

-- | Math size hint.
mathsize_ :: Text -> Attributes
mathsize_ = makeAttributes "mathsize"

-- | Semantic intent annotation reserved as valid by MathML Core.
intent_ :: Text -> Attributes
intent_ = makeAttributes "intent"

-- | Semantic argument annotation reserved as valid by MathML Core.
arg_ :: Text -> Attributes
arg_ = makeAttributes "arg"


-- | Math variant hint. This is a presentational hint that can be used to specify a particular font or style for an element.
-- MathML core only allows '"normal"' as a valid value, which can be used to override the default italic style for variables.
-- Other MathML implementations may handle more values. A more robust approach is to use a specific unicode code point to select the desired glyph.
mathvariant_ :: Text -> Attributes
mathvariant_ = makeAttributes "mathvariant"

-- * Basic MathML elements and attributes

-- | Group elements.
mrow_ :: Term arg result => arg -> result
mrow_ = term "mrow"

-- | Text embedded in MathML.
mtext_ :: Term arg result => arg -> result
mtext_ = term "mtext"

-- | Math identifier.
mi_ :: Term arg result => arg -> result
mi_ = term "mi"

-- | Math operators, including delimiters.
mo_ :: Term arg result => arg -> result
mo_ = term "mo"

-- | Math numbers.
mn_ :: Term arg result => arg -> result
mn_ = term "mn"

-- | String literals.
ms_ :: Term arg result => arg -> result
ms_ = term "ms"

-- | Blank space element with a size specified by its attributes. Takes
-- 'width_', 'height_', and 'depth_' attributes.
mspace_ :: Monad m => [Attributes] -> HtmlT m ()
mspace_ = makeElementNoEnd "mspace"

-- | Makes its content invisible. Can be used for alignment.
mphantom_ :: Term arg result => arg -> result
mphantom_ = term "mphantom"

-- | Style change container.
mstyle_ :: Term arg result => arg -> result
mstyle_ = term "mstyle"

-- | Error message box.
merror_ :: Term arg result => arg -> result
merror_ = term "merror"

-- | Adjust the size and position of its contents.
mpadded_ :: Term arg result => arg -> result
mpadded_ = term "mpadded"

-- | The @depth@ attribute. Useful in combination with 'mspace_' and 'mpadded_'.
-- See also 'Lucid.Html5.width_' and 'Lucid.Html5.height_'.
depth_ :: Text -> Attributes
depth_ = makeAttributes "depth"

-- | Horizontal offset. Useful in combination with 'mpadded_'.
lspace_ :: Text -> Attributes
lspace_ = makeAttributes "lspace"

-- | Vertical offset. Useful in combination with 'mpadded_'.
voffset_ :: Text -> Attributes
voffset_ = makeAttributes "voffset"


-- * Operator attributes

data OperatorForm
    = Prefix
    | Infix
    | Postfix
    deriving (Eq, Show)

-- | The @form@ attribute for operators.
form_ :: OperatorForm -> Attributes
form_ form = makeAttributes "form" form'
    where
        form' = case form of
            Prefix -> "prefix"
            Infix -> "infix"
            Postfix -> "postfix"


fence_ :: Bool -> Attributes
fence_ value = makeAttributes "fence" (if value then "true" else "false")

separator_ :: Bool -> Attributes
separator_ value = makeAttributes "separator" (if value then "true" else "false")

stretchy_ :: Bool -> Attributes
stretchy_ value = makeAttributes "stretchy" (if value then "true" else "false")

symmetric_ :: Bool -> Attributes
symmetric_ value = makeAttributes "symmetric" (if value then "true" else "false")

largeop_ :: Bool -> Attributes
largeop_ value = makeAttributes "largeop" (if value then "true" else "false")

movablelimits_ :: Bool -> Attributes
movablelimits_ value = makeAttributes "movablelimits" (if value then "true" else "false")

-- | Left and right operator spacing.
rspace_ :: Text -> Attributes
rspace_ = makeAttributes "rspace"

-- | Maximum stretched size.
maxsize_ :: Text -> Attributes
maxsize_ = makeAttributes "maxsize"

-- | Minimum stretched size.
minsize_ :: Text -> Attributes
minsize_ = makeAttributes "minsize"


-- * Accents, subscripts, multiscripts, fractions, and roots

-- | Generic @accent@ attribute helper.
accent_ :: Bool -> Attributes
accent_ value = makeAttributes "accent" (if value then "true" else "false")

accentunder_ :: Bool -> Attributes
accentunder_ value = makeAttributes "accentunder" (if value then "true" else "false")

-- | Fraction.
mfrac_ :: Term arg result => arg -> result
mfrac_ = term "mfrac"

-- | The @linethickness@ attribute. Useful in combination with 'mfrac_'.
linethickness_ :: Text -> Attributes
linethickness_ = makeAttributes "linethickness"

-- | Square root. For other radicals see 'mroot_'.
msqrt_ :: Term arg result => arg -> result
msqrt_ = term "msqrt"

-- | Generalized radical with arbitrary indices.
-- Its second child is the index of the radical.
mroot_ :: Term arg result => arg -> result
mroot_ = term "mroot"

-- | Place accents or limits above an element.
mover_ :: Term arg result => arg -> result
mover_ = term "mover"

-- | Place accents or limits below an element.
munder_ :: Term arg result => arg -> result
munder_ = term "munder"

-- | Place accents or limits below and above an element.
munderover_ :: Term arg result => arg -> result
munderover_ = term "munderover"

-- | Subscript.
msub_ :: Term arg result => arg -> result
msub_ = term "msub"

-- | Superscript.
msup_ :: Term arg result => arg -> result
msup_ = term "msup"

-- | Subscript and superscript.
msubsup_ :: Term arg result => arg -> result
msubsup_ = term "msubsup"

-- | Presubscripts and tensor notations.
mmultiscripts_ :: Term arg result => arg -> result
mmultiscripts_ = term "mmultiscripts"

-- | Switch to prescripts within a 'mmultiscripts_'.
mprescripts_ :: Monad m => [Attributes] -> HtmlT m ()
mprescripts_ = makeElementNoEnd "mprescripts"

-- | Legacy helper to skip a slot within a 'mmultiscripts_'.
-- This is not part of MathML Core, but it is harmless to expose.
none_ :: Monad m => [Attributes] -> HtmlT m ()
none_ = makeElementNoEnd "none"


-- * Tables and matrices

-- | Table or raw matrix.
mtable_ :: Term arg result => arg -> result
mtable_ = term "mtable"

-- | Table or matrix row.
mtr_ :: Term arg result => arg -> result
mtr_ = term "mtr"

-- | Table or matrix entry.
mtd_ :: Term arg result => arg -> result
mtd_ = term "mtd"

-- | The @columnspan@ attribute. Useful in combination with 'mtd_'.
-- See also 'Lucid.Html5.rowspan_'.
-- Note that MathML does not use HTML5's @colspan@.
columnspan_ :: Text -> Attributes
columnspan_ = makeAttributes "columnspan"


-- * Linking and actions

-- | Interactive action container.
maction_ :: Term arg result => arg -> result
maction_ = term "maction"

-- | The legacy @actiontype@ attribute for 'maction_'.
actiontype_ :: Text -> Attributes
actiontype_ = makeAttributes "actiontype"

-- | The legacy @selection@ attribute for 'maction_'.
selection_ :: Text -> Attributes
selection_ = makeAttributes "selection"


-- * Semantics

-- | Associate annotations with a MathML expression.
semantics_ :: Term arg result => arg -> result
semantics_ = term "semantics"

-- | Text annotations for 'semantics_'.
annotation_ :: Term arg result => arg -> result
annotation_ = term "annotation"

-- | XML annotations for 'semantics_'.
annotationXml_ :: Term arg result => arg -> result
annotationXml_ = term "annotation-xml"

encoding_ :: Text -> Attributes
encoding_ = makeAttributes "encoding"
