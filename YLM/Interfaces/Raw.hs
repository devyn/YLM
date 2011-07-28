module YLM.Interfaces.Raw (Raw(..)) where

import Prelude hiding (elem)
import YLM.Data
import Text.Parsec hiding ((<|>), optional, label, many)
import Control.Applicative
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List hiding (elem)
import Data.Foldable (foldlM)
import Data.Char (isDigit)

data Raw = Raw

instance TextInterface Raw where
  yread Raw fname text =
    case parse (optional whitespace *> elem `sepEndBy` optional whitespace <* eof) fname text of
      Left  err -> Left (show err)
      Right ell -> Right ell
  ywrite Raw es = intercalate "\n" $ map putRaw es

putRaw Nil                                          = "()"
putRaw i@(Cons _ _)                                 = concat ["(", f i, ")"]
  where f (Cons a (Cons b c))                       = concat [putRaw a, " ", f (Cons b c)]
        f (Cons a Nil)                              = putRaw a
        f (Cons a b)                                = concat [putRaw a, " . ", putRaw b]
putRaw (Label s)
  | null s                                          = "\"\""
  | s == "."                                        = "\".\""
  | all isDigit s                                   = '\"' : fmtEscape s ++ "\""
  | any (\x -> any (== x)
               " ()'\"\r\n\0\a\b\f\t\v\ESC\';.") s  = '\"' : fmtEscape s ++ "\""
  | otherwise                                       = s
putRaw (NumInt n)                                   = show n
putRaw (NumFloat n)                                 = show n
putRaw (Form s a w b)                               = putRaw $ Cons (Label "form") (Cons (Label a) (Cons (Label w) (Cons b Nil)))
putRaw (Lambda s as b)                              = putRaw $ Cons (Label "->") (Cons (atr as) (Cons b Nil))
putRaw (Action _)                                   = "!impure-action!"
putRaw (Window _)                                   = "!window!"
putRaw (Opaque idn _)                               = concat ["(opaque ", putRaw (Label idn), ")"]

fmtEscape (c : s) = (maybe [c] (('\\':).(: []).snd)
                     $ find ((== c).fst) (zip "\"\\\r\n\0\a\b\f\t\v\ESC\'" "\"\\rn0abftve'")) ++ fmtEscape s
fmtEscape []      = ""

instance PrettyPrint Raw where
  ypp Raw m o (e:[]) = prettyRaw m o e
  ypp Raw m o (e:es) = concat [prettyRaw m o e, "\n", take o $ repeat ' ', ypp Raw m o es]

prettyRaw m o Nil                           = "()"
prettyRaw m o i@(Cons a b)                  = case a of
                                                (Label l) -> 
                                                  if (length $ f m o i) > 100
                                                     then concat ["(", prettyRaw m o a
                                                                 ," ", cf m (o+length l+2) b, ")"]
                                                     else concat ["(", f m o i, ")"]
                                                _ ->
                                                  if (length $ f m o i) > 100
                                                     then concat ["(", cf m (o+1) i, ")"]
                                                     else concat ["(", f  m  o    i, ")"]
  where f  m o (Cons a (Cons b c))               = concat [prettyRaw m o a, " ", f m o (Cons b c)]
        f  m o (Cons a Nil)                      = prettyRaw m o a
        f  m o (Cons a b)                        = concat [prettyRaw m o a, " . ", prettyRaw m o b]
        cf m o (Cons a (Cons b c))               = concat [prettyRaw m o a, "\n"
                                                          ,take o $ repeat ' ', cf m o (Cons b c)]
        cf m o (Cons a Nil)                      = prettyRaw m o a
        cf m o (Cons a b)                        = concat [prettyRaw m o a, "\n", take o $ repeat ' '
                                                          ,". ", prettyRaw m o b]
prettyRaw m o (Label s)
  | null s                                      = concat ["\ESC[1;", c, "m\"\"\ESC[0m"]
  | s == "."                                    = concat ["\ESC[", c, "m\".\"\ESC[0m"]
  | all isDigit s                               = concat ["\ESC[", c, "m\"", fmtPrettyEscape c s, "\"\ESC[0m"]
  | any (\x -> any (== x)
               " ()'\"\r\n\0\a\b\f\t\v\ESC;") s = concat ["\ESC[", c, "m\"", fmtPrettyEscape c s, "\"\ESC[0m"]
  | otherwise                                   = concat ["\ESC[1;", c, "m", s, "\ESC[0m"]
  where c = if (length s > 2 && head s == '!' && last s == '!')
               then "31"
               else case (Map.lookup s m) of
                      Just (Right (Opaque _ _)) -> "36"
                      Just (Right _           ) -> "32"
                      Just (Left  _           ) -> "31"
                      Nothing                   -> "33"
prettyRaw m o (NumInt n)              = concat ["\ESC[36m", show n, "\ESC[0m"]
prettyRaw m o (NumFloat n)            = concat ["\ESC[36m", show n, "\ESC[0m"]
prettyRaw m o (Lambda s as b)         = if length sf > 100
                                           then lf
                                           else sf
  where sf = concat ["(\ESC[1;35m->\ESC[0m ", prettyRaw s (o+4) (atr as), " ", prettyRaw s (o+4) b, ")"]
        lf = concat ["(\ESC[1;35m->\ESC[0m ", prettyRaw s (o+4) (atr as), "\n"
                    ,take (o+4) $ repeat ' ', prettyRaw s (o+4) b, ")"]
prettyRaw m o (Form s a w b)          = if length sf > 100
                                           then lf
                                           else sf
  where sf = concat ["(\ESC[1;35mform\ESC[0m ", prettyRaw s (o+6) (Label a), " "
                    ,prettyRaw s (o+6) (Label w), " ", prettyRaw s (o+6) b, ")"]
        lf = concat ["(\ESC[1;35mform\ESC[0m ", prettyRaw s (o+6) (Label a), idt
                    ,prettyRaw s (o+6) (Label w), idt, prettyRaw s (o+6) b, ")"]
        idt = '\n' : take (o+6) (repeat ' ')
prettyRaw m o (Action _)              = prettyRaw m o (Label "!impure-action!")
prettyRaw m o (Window _)              = prettyRaw m o (Label "!window!")
prettyRaw m o (Opaque idc _)          = prettyRaw m o (Cons (Label "opaque") (Cons (Label idc) Nil))

fmtPrettyEscape :: String -> String -> String
fmtPrettyEscape c (ch : s) = maybe [ch] snd
                                   (find ((== ch).fst)
                                    (zip "\"\\\r\n\0\a\b\f\t\v\ESC" (map (("\ESC[0;35m\\"++).(:"\ESC["++c++"m"))
                                                                     "\"\\rn0abftve")))
                             ++ fmtPrettyEscape c s
fmtPrettyEscape c []       = ""

atr as = maybe (Label "!invalid-argument-list!") snd $ foldlM tf (0,Nil) (reverse as)
  where tf (0,x) y =
          case y of
            Rest     a -> Just (1, Label a)
            Optional a -> Just (1, Cons (Label $ "&" ++ a) x)
            Required a -> Just (2, Cons (Label a) x)
        tf (1,x) y =
          case y of
            Optional a -> Just (1, Cons (Label $ "&" ++ a) x)
            Required a -> Just (2, Cons (Label a) x)
            _          -> Nothing
        tf (2,x) y =
          case y of
            Required a -> Just (2, Cons (Label a) x)
            _          -> Nothing

elem, form, list, cons, num, int, float, label :: Parsec String () Elem
whitespace :: Parsec String () [String]
comment    :: Parsec String () String
escapeCode :: Parsec String () Char

elem = form <|> list <|> num <|> label

form = quote -- more forms coming soon (maybe)

quote = (\e -> Cons (Label "quote") (Cons e Nil)) <$> (char '\'' *> elem)

list = char '(' *> optional whitespace *> (cons <|> return Nil) <* optional whitespace <* char ')'

cons = Cons <$> (elem <* optional whitespace) <*> (try (char '.' *> optional whitespace *> elem) <|> cons <|> return Nil)

num = try float <|> int

int = try ((\ a b -> NumInt (read (a ++ b))) <$> string "0x" <*> many1 (oneOf (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])))
  <|> try ((\ a b -> NumInt (read (a ++ b))) <$> (maybeToList <$> optional (char '-')) <*> many1 (oneOf ['0'..'9']))

float = (\ a b c d ->
          NumFloat (read (a ++ b ++ c:d))) <$> (maybeToList <$> optional (char '-'))
                                              <*> many1 (oneOf ['0'..'9'])
                                              <*> char '.'
                                              <*> many1 (oneOf ['0'..'9'])

label = Label <$> f
  where f = try (char '"' *> many (noneOf "\"\\" <|> escapeCode) <* char '"')
            <|> many1 (noneOf " \t\r\n()'\";.")
            <|> many2 (noneOf " \t\r\n()'\";")

escapeCode = (\ c -> maybe '\0' snd $ find ((== c).fst) (zip "\"\\rn0abftve" "\"\\\r\n\0\a\b\f\t\v\ESC")) <$> (char '\\' *> oneOf "\"\\rn0abftve")

whitespace = many1 $ comment <|> many1 (oneOf " \t\r\n")

comment = char ';' *> many (noneOf "\n") <* (eof <|> (char '\n' *> pure ()))

many2 p = (:) <$> p <*> many1 p