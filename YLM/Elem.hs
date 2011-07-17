module YLM.Elem (Elem(..)) where

import Foreign.ForeignPtr

-- | The Elem  type contains the entire structure  of any YLM language
-- element.

data Elem = Nil                            -- ^ The empty list.
          | Cons     Elem Elem             -- ^ A list cons cell.
          | Label    String                -- ^ Any string literal or symbol word.
          | NumInt   Integer               -- ^ An integer numeral.
          | NumFloat Double                -- ^ A floating-point numeral.
          | Pointer  (ForeignPtr ()) Elem  -- ^ A pointer to a foreign data structure, with a proper serializable representation.
          deriving (Eq,Show)