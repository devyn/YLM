module YLM.PrettyPrint (PrettyPrint(..)) where

import YLM.Elem

class PrettyPrint a where
  -- | "Pretty prints" an Elem for use with a REPL. This pretty printing may do things
  -- like indenting and splitting long lines, or colorizing the output.
  ylmPrettyPrint :: a
                 -> Int    -- ^ The offset to indent each line except the first.
                 -> [Elem]
                 -> Either String String