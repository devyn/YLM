module YLM.Standard (standard) where

import YLM.Data
import YLM.Data.Util
import YLM.Runtime
import YLM.Interfaces.Raw
import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: set-scope; merge-window; concat; read-file; write-file; append-file;
--       load; explode; implode; map; left-fold; right-fold; empty; read-from-file

standard =
  Map.fromList [o "read"          oRead
               ,o "write"         oWrite
               ,o "pretty-print"  oPrettyPrint
               ,o "->"            oLambda
               ,o "form"          oForm
               ,o "self"          oSelf
               ,o "type-of"       oTypeOf
               ,o "let"           oLet
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
               ,t "and"           tAnd
               ,t "or"            tOr
               ,t "xor"           tXor
               ,o "="             oEq
               ,o "<"             oLessThan
               ,o ">"             oGreaterThan
               ,o "+"             oAdd
               ,o "-"             oSubtract
               ,o "*"             oMultiply
               ,o "/"             oDivide
               ,o "^"             oExponent
               ,o "list"          oList
               ,o "cons"          oCons
               ,o "head"          oHead
               ,o "tail"          oTail
               ,o "null?"         oNullQ
               ,o "do"            oDo
               ,o "bind"          oBind
               ,o "put-line"      oPutLine
               ,o "get-line"      oGetLine
               ,t "xyzzy"         tXyzzy]
  where o n f = (n, Right $ Opaque n f)
        t n v = (n, Right $ v)
        e n x = (n, Left  $ x)

oRead s (Cons a Nil) =
  do a' <- yeval s a
     case a' of
       Label st -> yread Raw "" st >>= Right . lcons
       _        -> tpe "label" a'

oWrite s ws
  | isPureList ws = Right $ Label $ ywrite Raw $ clist ws
  | otherwise     = fallback "0+" ws

oPrettyPrint s ws
  | isPureList ws = Right $ Label $ ypp Raw s 0 $ clist ws
  | otherwise     = fallback "0+" ws

oLambda s (Cons as (Cons b Nil)) =
  do al <- argList as
     return $ Lambda s al b
oLambda s _ =
  err ["malformed lambda expression."]

oForm s (Cons (Label a) (Cons (Label w) (Cons b Nil))) =
  Right $ Form s a w b
oForm s _ =
  err ["malformed special form expression."]

oSelf s Nil = Right $ Window s
oSelf s x   = fallback "0" x

oTypeOf s (Cons x Nil) = do x' <- yeval s x
                            Right $ Label $ ytype x'
oTypeOf s x            = fallback "1" x

oLet s (Cons (Cons (Label l) v) (Cons e Nil)) =
  oLet s (Cons (Cons (Cons (Label l) v) Nil) (Cons e Nil))
oLet s (Cons Nil (Cons e Nil)) =
  yeval s e
oLet s (Cons (Cons (Cons l v) xs) (Cons e Nil))
  | l == v = wcle
  | otherwise = 
    case l of
      Label k -> 
        let v' = yeval (Map.insert k v' s) v
        in oLet (Map.insert k v' s) (Cons xs (Cons e Nil))
      _ -> tpe "label" l
oLet s x = fallback "2" x

oDef s (Cons name (Cons value Nil))
  | name == value = wcle
  | otherwise =
    case name of
      Label k ->
        let v = yeval (Map.insert k v s) value
        in Right $ Action $ \ m -> return $ Right (Map.insert k v m, name)
      _ -> tpe "label" name
oDef s x = fallback "2" x

oUndef s (Cons name Nil) =
  case name of
    Label k ->
      if Map.member k s
         then Right $ Action $ \ m -> return $ Right $ (Map.delete k m, name)
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

tNot = Lambda Map.empty [Required "bool"] $ lcons [Label "bool", tFalse, tTrue]

tAnd = Lambda Map.empty [Required "a", Required "b"] $ lcons [Label "a", Label "b", tFalse]

tOr = Lambda Map.empty [Required "a", Required "b"] $ lcons [Label "a", tTrue, Label "b"]

tXor = Lambda Map.empty [Required "a", Required "b"] $ lcons [Label "a", lcons [Label "b", tFalse, tTrue], Label "b"]

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

oAdd = hMath sum sum

oSubtract = hMath subf subi
  where subf = foldl (-) 0
        subi = foldl (-) 0

oMultiply = hMath product product

oDivide = hMath divf divi
  where divf = foldl (/)    1
        divi = foldl (quot) 1

oExponent = hMath expf expi
  where expf = foldr (**) 1
        expi = foldr (^)  1

hMath ff fi s nsu
  | isPureList nsu =
    do ns <- mapM (yeval s) (clist nsu)
       if all (\ x -> case x of
                        NumFloat _ -> True
                        NumInt   _ -> True
                        _          -> False) ns
          then if any (\ x -> case x of
                                NumFloat _ -> True
                                _          -> False) ns
                  then Right $ NumFloat $ ff $ flip map ns $
                         \ x -> case x of
                                  NumFloat n -> n
                                  NumInt   n -> fromIntegral n
                  else Right $ NumInt $ fi $ flip map ns $
                         \ x -> case x of NumInt n -> n
          else tpem "numbers" ns
  | otherwise = fallback "0+" nsu

oList s x
  | isPureList x = mapM (yeval s) (clist x) >>= return . lcons
  | otherwise = fallback "0+" x

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

oDo s x
  | isPureList x = Right $ Action $ \ m ->
                     do e <- yexec s (clist x)
                        flip (either $ return . Left) e $
                          \ (s', r) -> return $ Right (m, r)
  | otherwise = fallback "0+" x

oBind s (Cons name (Cons avalue Nil))
  | name == avalue = wcle
  | otherwise =
    case name of
      Label k -> Right $ Action $ \ m -> do
        e <- yexec s [avalue]
        case e of
          Left _       -> return e
          Right (s',v) -> return $ Right (Map.insert k (Right v) m, name)
      _ -> tpe "label" name
oBind s x = fallback "2" x

oPutLine s (Cons l Nil) = do
  l' <- yeval s l
  case l' of
    Label str ->
      Right $ Action $ \ m -> do
        putStrLn str
        return $ Right (m, Nil)
    _ ->
      tpe "label" l'
oPutLine s x = fallback "1" x

oGetLine s Nil = Right $ Action $ \ m -> do
  ln <- getLine
  return $ Right (m, Label ln)
oGetLine s x = fallback "1" x

tXyzzy = Action $ \ m -> do putStrLn "\ESC[34m ~ Nothing happens.\ESC[0m"
                            return $ Right (m, Nil)


wcle = Left "I'm afraid I can't let you do that, for fear of the formation of a black hole."