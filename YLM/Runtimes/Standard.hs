module YLM.Runtimes.Standard (
  module YLM.Interfaces.Standard,
  standardLib
) where

import YLM.Elem
import YLM.Runtime
import YLM.TextInterface
import YLM.Interfaces.Standard
import qualified Control.Exception as E
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldlM)

instance Runtime Standard where
  -- Apply
  evaluate (Standard m) (Cons (Label l) b)                = maybe (Cons (Label l) b) (flip eApply (m,b))
                                                                  (Map.lookup l m)
  evaluate (Standard m) (Cons fn@(Cons (Label "->") x) d) = sapl fn (m,d)
  evaluate (Standard m) (Cons a b)                        = evaluate (Standard m)
                                                                     (Cons (evaluate (Standard m) a) b)
  evaluate (Standard m) (Label l)                         = maybe (Label l) eSerialize (Map.lookup l m)
  evaluate (Standard m) x                                 = x
  
  execute ri es = foldlM f (ri,Nil) es
    where f (r@(Standard m), _) (Cons (Label l) b) = if Map.member l m
                                                     then maybe (return (r, Nil))
                                                            (\ x -> do (m', b') <- eExecute x (m, b)
                                                                       return (Standard m', b'))
                                                            (Map.lookup l m)
                                                     else return (r, Cons (Label l) b)
          f (r@(Standard m), _) x                  = return (r, evaluate r x)

standardLib = Standard $ Map.fromList
                           [("put-line", StandardEntry { eSerialize = szNat "put-line"
                                                       , eApply     = aPutLine
                                                       , eExecute   = ePutLine })
                           ,("get-line", StandardEntry { eSerialize = szNat "get-line"
                                                       , eApply     = aGetLine
                                                       , eExecute   = eGetLine })
                           ,("quote",    StandardEntry { eSerialize = szNat "quote"
                                                       , eApply     = aQuote
                                                       , eExecute   = wrapE aQuote })
                           ,("error",    StandardEntry { eSerialize = szNat "error"
                                                       , eApply     = aError
                                                       , eExecute   = aError })
                           ,("def",      StandardEntry { eSerialize = szNat "def"
                                                       , eApply     = aDef
                                                       , eExecute   = eDef })
                           ,("+",        StandardEntry { eSerialize = szNat "+"
                                                       , eApply     = aAdd
                                                       , eExecute   = wrapE $ aAdd })
                           ,("-",        StandardEntry { eSerialize = szNat "-"
                                                       , eApply     = aSub
                                                       , eExecute   = wrapE $ aSub })
                           ,("*",        StandardEntry { eSerialize = szNat "*"
                                                       , eApply     = aMul
                                                       , eExecute   = wrapE $ aMul })
                           ,("/",        StandardEntry { eSerialize = szNat "/"
                                                       , eApply     = aDiv
                                                       , eExecute   = wrapE $ aDiv })
                           ,("^",        StandardEntry { eSerialize = szNat "^"
                                                       , eApply     = aExp
                                                       , eExecute   = wrapE $ aExp })
                           ,("->",       StandardEntry { eSerialize = szNat "->"
                                                       , eApply     = snd
                                                       , eExecute   = wrapE $ snd })
                           ,("true",     StandardEntry { eSerialize = sTrue
                                                       , eApply     = sapl  sTrue
                                                       , eExecute   = sexec sTrue })
                           ,("false",    StandardEntry { eSerialize = sFalse
                                                       , eApply     = sapl  sFalse
                                                       , eExecute   = sexec sFalse })
                           ,("read",     StandardEntry { eSerialize = szNat "read"
                                                       , eApply     = aRead
                                                       , eExecute   = wrapE $ aRead })
                           ,("write",    StandardEntry { eSerialize = szNat "write"
                                                       , eApply     = aWrite
                                                       , eExecute   = wrapE $ aWrite })]

aPutLine (m, x) = Cons (Label "put-line") x

ePutLine (m, Cons (Label x) Nil) = do putStrLn x
                                      return (m, Nil)
ePutLine (m, Cons (Label x) d)   = do putStrLn x
                                      ePutLine (m, d)
ePutLine (m, Nil)                = return (m, Nil)
ePutLine (m, b)                  = E.throw $ RuntimeException "type mismatch (expected: label...)"

aGetLine (m, x) = Cons (Label "get-line") x

eGetLine (m, _) = do l <- getLine
                     return (m, Label l)

aQuote (m, (Cons x Nil)) = x
aQuote (m, _)            = E.throw $ RuntimeException "type mismatch (expected: any)"

aError (m, Cons (Label l) Nil) = E.throw $ RuntimeException l
aError (m, _)                  = E.throw $ RuntimeException "type mismatch (expected: label)"

aDef (m, x) = Cons (Label "def") x

eDef (m, Cons (Label l) (Cons x Nil)) = return (Map.insert l (sdefent m x) m, Label l)

aAdd = mathematical (+) (+)    (NumInt 0)

aSub = mathematical (-) (-)    (NumInt 0)

aMul = mathematical (*) (*)    (NumInt 1)

aDiv = mathematical (/) (quot) (NumInt 1)

aExp (m,(Cons c d)) = mathematicalr (flip (**)) (flip (^)) c (m,d)
aExp (m,Nil)        = NumInt 1

sTrue  = ltc [Label "->", ltc [Label "a", Label "b"], Label "a"]

sFalse = ltc [Label "->", ltc [Label "a", Label "b"], Label "b"]

aRead (m, (Cons (Label l) Nil)) =
  case ylmRead (Standard m) l of
    Left err     -> E.throw $ RuntimeException err
    Right []     -> Nil
    Right (x:[]) -> x
    Right xs     -> ltc xs
aRead (m, (Cons b Nil)) =
  case evaluate (Standard m) b of
    Label l -> aRead (m, (Cons (Label l) Nil))
    _       -> E.throw $ RuntimeException "type mismatch (expected: label)"
aRead (m, _) = E.throw $ RuntimeException "type mismatch (expected: label)"

aWrite (m, xs) =
  case ylmWrite (Standard m) (map (evaluate (Standard m)) $ ctl xs) of
    Left err -> E.throw $ RuntimeException err
    Right s  -> Label s

szNat s = Cons (Label "native") (Cons (Label s) Nil)

wrapE f (m, x) = return (m, f (m, x))

sfun args body (m, x) = if length args == length ia
                          then evaluate (Standard $ Map.union bv m) body
                          else E.throw $ RuntimeException ("expected " ++ show (length args)
                                                           ++ " argument(s), got " ++ show (length ia))
  where ia = ctl x
        bv = Map.fromList $ zip (map (\ (Label l) -> l) args) (map (sdefent m) ia)

sdefent :: Map String StandardEntry -> Elem -> StandardEntry

sdefent m x = StandardEntry { eSerialize = ex
                            , eApply     = sapl ex
                            , eExecute   = sexec ex }
  where ex = evaluate (Standard m) x

sapl fb@(Cons (Label "->")
              (Cons ar (Cons fn Nil))) (m,x) = sfun (ctl ar) fn (m,x)
sapl b (m,x)  = E.throw $ RuntimeException $ "type mismatch: (" ++
                          either (const "!write-error") id (ylmWrite standardLib [b]) ++
                          " is not a callable form)"

sexec b (m,a) = return (m,sapl b (m,a))

ctl Nil = []
ctl (Cons h t) = h : ctl t

ltc [] = Nil
ltc (h:t) = Cons h (ltc t)

mathematical :: (Double  -> Double  -> Double )
             -> (Integer -> Integer -> Integer)
             -> Elem
             -> (Map String StandardEntry, Elem)
             -> Elem

mathematical ff fi ss (m,a) = foldl f ss (ctl a)
  where f s e = let ee = evaluate (Standard m) e
                in case (s,ee) of
                  (NumInt   si, NumInt   ni) -> NumInt   $ si `fi` ni
                  (NumInt   si, NumFloat nf) -> NumFloat $ fromIntegral si `ff` nf
                  (NumFloat sf, NumInt   ni) -> NumFloat $ sf `ff` fromIntegral ni
                  (NumFloat sf, NumFloat nf) -> NumFloat $ sf `ff` nf
                  (_, _)                     -> ltc [Label "error", Label "type error"]

mathematicalr :: (Double  -> Double  -> Double )
              -> (Integer -> Integer -> Integer)
              -> Elem
              -> (Map String StandardEntry, Elem)
              -> Elem

mathematicalr ff fi ss (m,a) = foldr f ss (ctl a)
  where f s e = let ee = evaluate (Standard m) e
                in case (s,ee) of
                  (NumInt   si, NumInt   ni) -> NumInt   $ si `fi` ni
                  (NumInt   si, NumFloat nf) -> NumFloat $ fromIntegral si `ff` nf
                  (NumFloat sf, NumInt   ni) -> NumFloat $ sf `ff` fromIntegral ni
                  (NumFloat sf, NumFloat nf) -> NumFloat $ sf `ff` nf
                  (_, _)                     -> ltc [Label "error", Label "type error"]