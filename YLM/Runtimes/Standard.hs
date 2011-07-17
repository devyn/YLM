module YLM.Runtimes.Standard (
  module YLM.Interfaces.Standard
) where

import YLM.Elem
import YLM.Runtime
import YLM.Interfaces.Standard
import qualified Control.Exception as E

instance Runtime Standard where
  -- Multiple-stage solver.
  evaluate Standard (Cons (Label "quote") (Cons a Nil)) = a
  evaluate Standard x = let r  = f x
                            r' = f r
                        in if r == r'
                             then r'
                             else evaluate Standard r'
    where f (Cons (Label "+") c)          = foldl (mathematical (+)  (+))    (NumInt 0) (ctl c)
          f (Cons (Label "-") c)          = foldl (mathematical (-)  (-))    (NumInt 0) (ctl c)
          f (Cons (Label "*") c)          = foldl (mathematical (*)  (*))    (NumInt 1) (ctl c)
          f (Cons (Label "/") c)          = foldr (mathematical (/)  (quot)) (NumInt 1) (ctl c)
          f (Cons (Label "^") (Cons c d)) = foldl (mathematical (**) (^))    c          (ctl d)
          f x = x

  execute Standard ((Cons (Label "quote") (Cons a Nil)) : []) = return a
  execute Standard es = do r  <- f es
                           r' <- f [r]
                           if r == r'
                             then return r'
                             else execute Standard [r']
    where f [] = return Nil
          f ((Cons (Label "error") (Cons (Label e) Nil)) : _) = E.throw (RuntimeException e)
          f ((Cons (Label "print") (Cons (Label m) Nil)) : x) = putStrLn m >> execute Standard x
          f ((Cons (Label "get-line") Nil) : x) = do l <- getLine
                                                     if null x
                                                       then return (Label l)
                                                       else execute Standard x
          f es = return $ last $ map (evaluate Standard) es

ctl Nil = []
ctl (Cons h t) = h : ctl t

ltc [] = Nil
ltc (h:t) = Cons h (ltc t)

mathematical :: (Double  -> Double  -> Double )
             -> (Integer -> Integer -> Integer)
             -> Elem
             -> Elem
             -> Elem

mathematical ff fi s e = let ee = evaluate Standard e
                     in case (s,ee) of
                       (NumInt   si, NumInt   ni) -> NumInt   $ si `fi` ni
                       (NumInt   si, NumFloat nf) -> NumFloat $ fromIntegral si `ff` nf
                       (NumFloat sf, NumInt   ni) -> NumFloat $ sf `ff` fromIntegral ni
                       (NumFloat sf, NumFloat nf) -> NumFloat $ sf `ff` nf
                       (_, _)                     -> ltc [Label "error", Label "type error"]