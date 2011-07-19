module YLM.Interfaces.Standard (Standard(..), StandardEntry (..)) where

import Prelude hiding (elem)
import YLM.Elem
import YLM.TextInterface
import YLM.PrettyPrint
import Text.Parsec hiding ((<|>), optional, label, many)
import Control.Applicative
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find)

data Standard = Standard (Map String StandardEntry)

data StandardEntry = StandardEntry {
  eSerialize :: Elem,
  eApply     :: (Map String StandardEntry, Elem) -> Either String Elem,
  eExecute   :: (Map String StandardEntry, Elem) -> IO (Either String (Map String StandardEntry, Elem))
}

instance Show Standard where
  show (Standard m) = "Standard :" ++ show (Map.size m)

instance TextInterface Standard where
  ylmRead (Standard _) [] = Right []
  ylmRead (Standard _) text =
    case parse (optional whitespace *>
                elem `sepBy` optional whitespace
                <* optional whitespace <* eof) "" text of
      Left  err -> Left (show err)
      Right ell -> Right ell
  ylmWrite   (Standard _) []     = Right $ ""
  ylmWrite   (Standard _) (e:[]) = Right $ putStandard e
  ylmWrite r@(Standard _) (e:es) = either Left (\x -> Right $ (putStandard e) ++ "\n" ++ x) $ ylmWrite r es

putStandard (Cons (Label "quote")
                  (Cons x Nil))   = '\'' : putStandard x
putStandard Nil                   = "()"
putStandard i@(Cons _ _)          = "(" ++ f i ++ ")"
  where f (Cons a (Cons b c))     = putStandard a ++ " " ++ f (Cons b c)
        f (Cons a Nil)            = putStandard a
        f (Cons a b)              = putStandard a ++ " . " ++ putStandard b
putStandard (Label s)
  | null s                        = "\"\""
  | any (\x -> any (== x)
               " ()'\"\r\n\0") s  = '\"' : fmtEscape s ++ "\""
  | otherwise                     = s
putStandard (NumInt n)            = show n
putStandard (NumFloat n)          = show n
putStandard (Pointer p e)         = putStandard e

fmtEscape (c : s) = (maybe [c] (('\\':).(: []).snd)
                     $ find ((== c).fst) (zip "\"\\\r\n\0\a\b\f\t\v\ESC\'" "\"\\rn0abftve'")) ++ fmtEscape s
fmtEscape []      = []

instance PrettyPrint Standard where
  ylmPrettyPrint (Standard m) o (e:[]) = Right $ prettyStandard m o e
  ylmPrettyPrint (Standard m) o (e:es) = either Left (\x ->
                                                       Right $ (prettyStandard m o e) ++ "\n"
                                                               ++ (take o $ repeat ' ') ++ x)
                                           $ ylmPrettyPrint (Standard m) o es
  
prettyStandard m o (Cons (Label "quote")
                         (Cons x Nil))         = "\ESC[35m\'\ESC[0m" ++ prettyStandard m o x
prettyStandard m o Nil                         = "()"
prettyStandard m o i@(Cons a b)                = case a of
                                                   (Label l) -> 
                                                     if (length $ f m o i) > 75
                                                       then "(" ++ prettyStandard m o a ++ " " ++ cf m (o+length l+2) b ++ ")"
                                                       else "(" ++ f m o i ++ ")"
                                                   _ ->
                                                     if (length $ f m o i) > 75
                                                       then "(" ++ cf m (o+1) i ++ ")"
                                                       else "(" ++ f  m  o    i ++ ")"
  where f  m o (Cons a (Cons b c))             = prettyStandard m o a ++ " " ++ f m o (Cons b c)
        f  m o (Cons a Nil)                    = prettyStandard m o a
        f  m o (Cons a b)                      = prettyStandard m o a ++ " . " ++ prettyStandard m o b
        cf m o (Cons a (Cons b c))             = prettyStandard m o a ++ "\n" ++ (take o $ repeat ' ') ++ cf m o (Cons b c)
        cf m o (Cons a Nil)                    = prettyStandard m o a
        cf m o (Cons a b)                      = prettyStandard m o a ++ "\n" ++ (take o $ repeat ' ') ++ ". " ++ prettyStandard m o b
prettyStandard m o (Label s)
  | null s                                     = "\ESC[1;"++c++"m\"\"\ESC[0m"
  | any (\x -> any (== x)
               " ()'\"\r\n\0\a\b\f\t\v\ESC") s = "\ESC["++c++"m\"" ++ fmtPrettyEscape c s ++ "\"\ESC[0m"
  | otherwise                                  = "\ESC[1;"++c++"m" ++ s ++ "\ESC[0m"
  where c = if (Map.member s m)
              then "32"
              else "33"
prettyStandard m o (NumInt n)              = "\ESC[36m" ++ show n ++ "\ESC[0m"
prettyStandard m o (NumFloat n)            = "\ESC[36m" ++ show n ++ "\ESC[0m"
prettyStandard m o (Pointer p e)           = prettyStandard m o e

fmtPrettyEscape :: String -> String -> String
fmtPrettyEscape c (ch : s) = maybe [ch] snd (find ((== ch).fst)
                                            (zip "\"\\\r\n\0\a\b\f\t\v\ESC" (map (("\ESC[0;35m\\"++).(:"\ESC["++c++"m")) "\"\\rn0abftve")))
                             ++ fmtPrettyEscape c s
fmtPrettyEscape c []       = []

elem, form, list, cons, num, int, float, label :: Parsec String () Elem
whitespace :: Parsec String () String
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
            <|> many1 (noneOf " ()'\"")

escapeCode = (\ c -> maybe '\0' snd $ find ((== c).fst) (zip "\"\\rn0abftve" "\"\\\r\n\0\a\b\f\t\v\ESC")) <$> (char '\\' *> oneOf "\"\\rn0abftve")

whitespace = many1 (oneOf " \t\r\n")