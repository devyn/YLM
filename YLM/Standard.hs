module YLM.Standard (standard) where

import YLM.Data
import YLM.Data.Util
import YLM.Runtime
import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: +; -; *; /; ^; read; write; bind;
--       let; list; do; load; explode; implode

standard =
  Map.fromList [o "->"            oLambda
               ,o "form"          oForm
               ,o "self"          oSelf
               ,o "def"           oDef
               ,o "undef"         oUndef
               ,o "list-defined"  oListDefined
               ,o "quote"         oQuote
               ,o "unquote"       oUnquote
               ,o "error"         oError
               ,t "id"            tId
               ,t "true"          tTrue
               ,t "false"         tFalse
               ,t "not"           tNot
               ,o "="             oEq
               ,o "<"             oLessThan
               ,o ">"             oGreaterThan
               ,o "cons"          oCons
               ,o "head"          oHead
               ,o "tail"          oTail
               ,o "null?"         oNullQ
               ,o "put-line"      oPutLine
               ,o "get-line"      oGetLine]
  where o n f = (n, Opaque n f)
        t n v = (n, v)

oLambda s (Cons as (Cons b Nil)) =
  do al <- argList as
     return $ Lambda s al b
oLambda s _ =
  err ["malformed lambda expression."]

oForm s (Cons (Label a) (Cons (Label w) (Cons b Nil)))
  = Right $ Form s a w b
oForm s _
  = err ["malformed special form expression."]

oSelf s Nil = Right $ Window s
oSelf s x   = fallback "0" x

oDef s (Cons name (Cons value Nil)) = do
  v <- yeval s value
  case name of
    Label k -> Right $ Action $ return $ Right $ (Map.insert k v s, name)
    _       -> tpe "label" name
oDef s x = fallback "2" x

oUndef s (Cons name Nil) =
  case name of
    Label k ->
      if Map.member k s
         then Right $ Action $ return $ Right $ (Map.delete k s, name)
         else err ["`", k, "' is not defined."]
    _ -> tpe "label" name
oUndef s x = fallback "1" x

oListDefined s Nil =
  Right $ foldl (\ o x -> Cons (Label x) o) Nil (reverse $ Map.keys s)
oListDefined s x = fallback "0" x

oQuote s (Cons x Nil) = Right x
oQuote s x            = fallback "1" x

oUnquote s (Cons x Nil) = yeval s x >>= yeval s
oUnquote s x            = fallback "1" x

oError s (Cons x Nil) =
  do x' <- yeval s x
     case x' of
       Label err -> Left err
       _         -> tpe "label" x'
oError s x = fallback "1" x

tId = Lambda Map.empty [Required "a"] (Label "a")

tTrue = Lambda Map.empty [Required "a", Required "b"] (Label "a")

tFalse = Lambda Map.empty [Required "a", Required "b"] (Label "b")

tNot = Lambda Map.empty [Required "bool"] (Cons (Label "bool") (Cons tFalse (Cons tTrue Nil)))

oEq s (Cons a (Cons b Nil)) = do
  a' <- yeval s a
  b' <- yeval s b
  if a' == b'
     then Right tTrue
     else Right tFalse
oEq s x = fallback "2" x

oLessThan s (Cons a (Cons b Nil)) = do
  a' <- yeval s a
  b' <- yeval s b
  if ytype a' == ytype b'
     then case (a', b') of
            (Label la, Label lb) -> Right $ if la < lb then tTrue else tFalse
            (Nil, Cons bh bt)    -> Right tTrue
            (Cons ah at, Nil)    -> Right tFalse
            (la@(Cons _ _), lb@(Cons _ _))
              | isPureList la && isPureList lb -> Right $ if sizeCons la < sizeCons lb then tTrue else tFalse
              | otherwise                      -> err ["can't compare dirty lists."]
            (NumInt ia, NumInt ib) -> Right $ if ia < ib then tTrue else tFalse
            (NumFloat fa, NumFloat fb) -> Right $ if fa < fb then tTrue else tFalse
            (NumInt ia, NumFloat fb) -> Right $ if (fromIntegral ia) < fb then tTrue else tFalse
            (NumFloat fa, NumInt ib) -> Right $ if fa < (fromIntegral ib) then tTrue else tFalse
            (x, y) -> tpem "labels, lists, or numbers" [x, y]
     else err ["type error: first and second argument must be of the same type."]
oLessThan s x = fallback "2" x

oGreaterThan s (Cons a (Cons b Nil)) = oLessThan s (Cons b (Cons a Nil))
oGreaterThan s x                     = fallback "2" x

oCons s (Cons i (Cons l Nil)) = do
  i' <- yeval s i
  l' <- yeval s l
  Right $ Cons i' l'
oCons s x = fallback "2" x

oHead s (Cons l Nil) = do
  l' <- yeval s l
  case l' of
    (Cons x _) -> Right x
    Nil        -> err ["empty list."]
    x          -> tpe "list" x
oHead s x = fallback "1" x

oTail s (Cons l Nil) = do
  l' <- yeval s l
  case l' of
    (Cons _ x) -> Right x
    Nil        -> err ["empty list."]
    x          -> tpe "list" x
oTail s x = fallback "1" x

oNullQ s (Cons l Nil) = do
    l' <- yeval s l
    case l' of
      Nil      -> Right tTrue
      Cons _ _ -> Right tFalse
      _        -> tpe "list" l'
oNullQ s x = fallback "1" x

oPutLine s (Cons l Nil) = do
  l' <- yeval s l
  case l' of
    Label str ->
      Right $ Action $ do
        putStrLn str 
        return $ Right (s, Nil)
    _ ->
      tpe "label" l'
oPutLine s x = fallback "1" x

oGetLine s Nil = Right $ Action $ do
  ln <- getLine
  return $ Right (s, Label ln)
oGetLine s x = fallback "1" x