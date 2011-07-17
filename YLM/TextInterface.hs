module YLM.TextInterface (TextInterface(..)) where

import YLM.Elem

class TextInterface a where
  ylmRead  :: a -> String -> Either String [Elem]
  ylmWrite :: a -> [Elem] -> Either String String