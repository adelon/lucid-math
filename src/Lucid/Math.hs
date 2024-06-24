{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Math
    ( module Lucid.Math
    -- * Re-exports from "Lucid.Html5".
    , module Export
    ) where


import Data.Text (Text)
import Data.Text qualified as Text

import Lucid.Base
import Lucid.Html5 as Export (height_, width_, rowspan_)

-- * The top-level @math@ MathML element

-- | Top-level element for MathML content. All
-- mathematical content must be wrapped by this element.
-- There should not be a nested 'math_' within this element.
math_ :: Term arg result => arg -> result
math_ = term "math"

-- | Displays the element outside the current span of text as
-- a separate block. Analogous to LaTeX's @\\[..\\]@.
displayBlock_ :: Attributes
displayBlock_ = makeAttributes "display" "block"

-- | Displays an element within the current span of text.
-- This is the default value. Analogous to LaTeX's @\\(..\\)@ and @$...$@.
displayInline_ :: Attributes
displayInline_ = makeAttributes "display" "inline"


-- | The @linethickness@ attribute. Useful in combination with 'mfrac_'.
linethickness_ :: Text -> Attributes
linethickness_ = makeAttributes "linethickness"

-- * Basic MathML elements and attributes

-- | Group elements.
mrow_ :: Term arg result => arg -> result
mrow_ = term "mrow"

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

-- | Blank space element with a size specified by its attributes.
-- Takes 'width_', 'height_', and 'depth_' attributes.
mspace_ :: Monad m => [Attributes] -> HtmlT m ()
mspace_ = makeElementNoEnd "mprescripts"

-- | Makes its content invisible. Can be used for alignment.
mphantom_ :: Term arg result => arg -> result
mphantom_ = term "mphantom"

-- | The @depth@ attribute. Useful in combination with 'mspace_',
-- See also 'Lucid.Html5.width_' and 'Lucid.Html5.height_'.
depth_ :: Text -> Attributes
depth_ = makeAttributes "depth"


-- * Global attributes

-- | Presentational hint that the height of the formula should not be minimized.
-- Corresponds to @displaystyle="true"@.
displayStyle_ :: Attributes
displayStyle_ = makeAttributes "displaystle" "true"

-- | Presentational hint that the height of the formula should be minimized.
-- Corresponds to @displaystyle="false"@.
compactStyle_ :: Attributes
compactStyle_ = makeAttributes "compactStyle" "true"

-- | Presentational hint for the @math-depth@ of an element. See [the MathML spec](https://w3c.github.io/mathml-core/#dfn-scriptlevel).
scriptLevel_ :: Int -> Attributes
scriptLevel_ k = makeAttributes "compactStyle" (Text.pack (show k))


variant_ :: Text -> Attributes
variant_ = makeAttributes "mathvariant"

variantNormal_ :: Attributes
variantNormal_ = makeAttributes "mathvariant" "normal"

variantBold_ :: Attributes
variantBold_ = makeAttributes "mathvariant" "bold"

variantItalic_ :: Attributes
variantItalic_ = makeAttributes "mathvariant" "italic"

variantBoldItalic_ :: Attributes
variantBoldItalic_ = makeAttributes "mathvariant" "bold-italic"

variantSansSerif_ :: Attributes
variantSansSerif_ = makeAttributes "mathvariant" "sans-serif"

variantSansSerifBold_ :: Attributes
variantSansSerifBold_ = makeAttributes "mathvariant" "bold-sans-serif"

variantSansSerifItalic_ :: Attributes
variantSansSerifItalic_ = makeAttributes "mathvariant" "sans-serif-italic"

variantSansSerifBoldItalic :: Attributes
variantSansSerifBoldItalic = makeAttributes "mathvariant" "sans-serif-bold-italic"

variantFraktur :: Attributes
variantFraktur = makeAttributes "mathvariant" "fraktur"

variantFrakturBold :: Attributes
variantFrakturBold = makeAttributes "mathvariant" "bold-fraktur"

variantScript_ :: Attributes
variantScript_ = makeAttributes "mathvariant" "script"

variantScriptBold_ :: Attributes
variantScriptBold_ = makeAttributes "mathvariant" "bold-script"

variantDoubleStruck_ :: Attributes
variantDoubleStruck_ = makeAttributes "mathvariant" "double-struck"

variantMonospace_ :: Attributes
variantMonospace_ = makeAttributes "mathvariant" "monospace"

variantInitial_ :: Attributes
variantInitial_ = makeAttributes "mathvariant" "initial"

variantTailed_ :: Attributes
variantTailed_ = makeAttributes "mathvariant" "tailed"

variantLooped_ :: Attributes
variantLooped_ = makeAttributes "mathvariant" "looped"

variantStretched_ :: Attributes
variantStretched_ = makeAttributes "mathvariant" "stretched"



-- * Accents, subscripts, multiscripts, fractions, and roots

-- | Attributes for 'munder_', 'mover_', and 'munderover_'.
-- Prevents down-scaling.
accent_ :: Attributes
accent_ = makeAttributes "accent" "true"

-- | Attributes for 'munderover_'.
-- Prevents down-scaling.
accentUnder_ :: Attributes
accentUnder_ = makeAttributes "accentunder" "true"

-- | Fraction.
mfrac_ :: Term arg result => arg -> result
mfrac_ = term "mfrac"

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

-- | Can be used to skip a slot within a 'mmultiscripts_'.
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
-- Note that MathML does not use HTML5's 'Lucid.Html5.colspan_'.
columnspan_ :: Text -> Attributes
columnspan_ = makeAttributes "depth"


-- * Semantics

-- | Associate annotations with a MathML expression.
semantics_ :: Term arg result => arg -> result
semantics_ = term "semantics"

-- | Text annotations for 'semantics_'.
annotation_ :: Term arg result => arg -> result
annotation_ = term "annotation"

-- | Annotations for 'semantics_' in TEX syntax.
annotationTex_ :: Monad m => HtmlT m a -> HtmlT m a
annotationTex_ = annotation_ [encodingTex_]

-- | Specify the encoding for an 'annotation_'.
encoding_ :: Text -> Attributes
encoding_ = makeAttributes "encoding"

-- | For an 'annotation_' in TEX syntax.
encodingTex_ :: Attributes
encodingTex_ = makeAttributes "encoding" "application/x-tex"
