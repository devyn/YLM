module YLM.Interfaces.Standard (Standard(..)) where

import Prelude hiding (elem)
import YLM.Elem
import YLM.TextInterface
import Text.Parsec hiding ((<|>), optional, label, many)
import Control.Applicative
import Data.Maybe

data Standard = Standard deriving (Show)

instance TextInterface Standard where
  ylmRead Standard [] = Right []
  ylmRead Standard text =
    case parse (optional whitespace *>
                elem `sepBy` optional whitespace
                <* optional whitespace <* eof) "" text of
      Left  err -> Left (show err)
      Right ell -> Right ell
  ylmWrite Standard []     = Right $ ""
  ylmWrite Standard (e:[]) = Right $ putStandard e
  ylmWrite Standard (e:es) = either Left (\x -> Right $ (putStandard e) ++ "\n" ++ x) $ ylmWrite Standard es

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
               " ()\"\r\n\0") s   = '\"' : fmtEscape s ++ "\""
  | otherwise                     = s
putStandard (NumInt n)            = show n
putStandard (NumFloat n)          = show n
putStandard (Pointer p e)         = putStandard e

fmtEscape ('\"' : s) = "\\\"" ++ fmtEscape s
fmtEscape ('\r' : s) = "\\r"  ++ fmtEscape s
fmtEscape ('\n' : s) = "\\n"  ++ fmtEscape s
fmtEscape ('\0' : s) = "\\0"  ++ fmtEscape s
fmtEscape (c    : s) = c      :  fmtEscape s
fmtEscape []         = []

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
            <|> many1 (noneOf " ()\"")

escapeCode = f <$> (char '\\' *> oneOf "\"\\rn0")
  where f '\"' = '\"'
        f '\\' = '\\'
        f 'r'  = '\r'
        f 'n'  = '\n'
        f '0'  = '\0'

whitespace = many1 (oneOf " \t\r\n")