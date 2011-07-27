module YLM.Data (
  Elem(..),
  Argument(..),
  Scope,
  TextInterface(..),
  PrettyPrint(..),
  ytype
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 (ByteString)

-- Note: The first argument to the Native constructor is the name it can be found by in the Scope
--       and is also used in comparisons. That way [as an example in Standard] (= put-line put-line)
--       will still return (-> (a b) a) [which is true].

data Elem = Nil
          | Cons Elem Elem
          | Label String
          | NumInt Integer
          | NumFloat Double
          | Form Scope String String Elem
          | Lambda Scope [Argument] Elem
          | Action (IO (Either String (Scope, Elem)))
          | Opaque String (Scope -> Elem -> Either String Elem)
          | Window Scope

instance Show Elem where
  showsPrec d Nil = showString "Nil"
  showsPrec d (Cons a b) = showParen (d > 10) $
    showString "Cons " . showsPrec 11 a
    . showString " " . showsPrec 11 b
  showsPrec d (Label s) = showParen (d > 10) $
    showString "Label " . showsPrec 11 s
  showsPrec d (NumInt i) = showParen (d > 10) $
    showString "NumInt " . showsPrec 11 i
  showsPrec d (NumFloat f) = showParen (d > 10) $
    showString "NumFloat " . showsPrec 11 f
  showsPrec d (Form s a w b) = showParen (d > 10) $
    showString "Form " . showsPrec 11 s
    . showString " " . showsPrec 11 a
    . showString " " . showsPrec 11 w
    . showString " " . showsPrec 11 b
  showsPrec d (Lambda s as b) = showParen (d > 10) $
    showString "Lambda <Scope> " . showsPrec 10 as
    . showString " -> " . showsPrec 10 b
  showsPrec d (Action _) = showString "<Action>"
  showsPrec d (Opaque idn _) = showParen (d > 10) $
    showString "Opaque " . showsPrec 11 idn . showString " (->)"
  showsPrec d (Window s) = showParen (d > 10) $
    showString "Window " . showsPrec 11 s

instance Eq Elem where
  Nil == Nil = True
  Cons a b == Cons x y = a == x && b == y
  Label a == Label b = a == b
  NumInt a == NumInt b = a == b
  NumFloat a == NumFloat b = a == b
  Form s1 a1 w1 b1 == Form s2 a2 w2 b2 =
    s1 == s2 && a1 == a2 && w1 == w2 && b1 == b2
  Lambda s1 as1 b1 == Lambda s2 as2 b2 =
    s1 == s2 && as1 == as2 && b1 == b2
  Opaque a _ == Opaque b _ = a == b
  Window a == Window b = a == b
  _ == _ = False

ytype :: Elem -> String

ytype Nil             = "list"
ytype (Cons _ _)      = "list"
ytype (Label _)       = "label"
ytype (NumInt _)      = "number"
ytype (NumFloat _)    = "number"
ytype (Form _ _ _ _)  = "function"
ytype (Lambda _ _ _)  = "function"
ytype (Opaque _ _)    = "function"
ytype (Window _)      = "function"
ytype (Action _)      = "action"

data Argument = Required String
              | Optional String
              | Rest     String
              deriving (Show, Eq)

type Scope = Map String Elem

class TextInterface a where
  yread  :: a                    -- ^ Text interface to use
         -> String               -- ^ File name
         -> String               -- ^ Input text
         -> Either String [Elem] -- ^ Either an error or our result

  ywrite :: a                    -- ^ Text interface to use
         -> [Elem]               -- ^ Input elements
         -> String               -- ^ Output written string (writer can't fail)

class PrettyPrint a where
  ypp :: a -> Scope -> Int -> [Elem] -> String