module YLM.Data.Util (err, argList, arity, addArity, showArity, sizeCons, isPureList, fallback, tpe, tpem, isCons, lcons) where

import YLM.Data
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

err :: [[a]] -> Either [a] b

err = Left . concat

argList :: Elem -> Either String [Argument]

argList = f 0
  where f n (Cons (Label x) y)
          | length x > 1
            && head x == '&' = either Left (Right . ((Optional x) :)) $ f 1 y
          | otherwise        = if n > 0
                                  then Left "required arguments must come before optional arguments."
                                  else either Left (Right . ((Required x) :)) $ f 0 y
        f n (Label x) = Right [Rest x]
        f n Nil       = Right []
        f n _         = Left "argument list must contain only labels (variable names to bind to)"

arity :: [Argument] -> (Int, Maybe Int)

arity ((Required _):xs) = (1, Just  1) `addArity` arity xs
arity ((Optional _):xs) = (0, Just  1) `addArity` arity xs
arity ((Rest     _):_ ) = (0, Nothing)
arity []                = (0, Just 0)

addArity :: (Int, Maybe Int) -> (Int, Maybe Int) -> (Int, Maybe Int)

addArity (n1, Just m1) (n2, Just m2) = (n1 + n2, Just $ m1 + m2)
addArity (n1, Nothing) (n2, _)       = (n1 + n2, Nothing)
addArity (n1, _)       (n2, Nothing) = (n1 + n2, Nothing)

showArity :: (Int, Maybe Int) -> String

showArity (n, Nothing) = show n ++ "+"
showArity (n, Just m)
  | n == m    = show n
  | otherwise = concat [show n, "-", show m]

sizeCons :: Elem -> Int

sizeCons (Cons _ xs) = 1 + sizeCons xs
sizeCons Nil = 0

isPureList :: Elem -> Bool

isPureList (Cons _ xs) = isPureList xs
isPureList Nil         = True
isPureList _           = False

fallback :: String -> Elem -> Either String Elem

fallback n x
  | isPureList x = err ["wrong number of arguments (expected ", n, " argument(s), got ", show $ sizeCons x, ")."]
  | otherwise    = err ["can't cons that to this type of function!"]

tpe :: String -> Elem -> Either String Elem

tpe ex g = err ["type error (expected ", ex, ", but got ", ytype g, ")."]

tpem :: String -> [Elem] -> Either String Elem

tpem ex gs = err ["type error (expected ", ex, ", but got [", intercalate ", " (map ytype gs), "])."]

isCons :: Elem -> Bool

isCons Nil        = True
isCons (Cons _ _) = True
isCons _          = False

lcons :: [Elem] -> Elem
lcons (x:xs) = Cons x (lcons xs)
lcons []     = Nil