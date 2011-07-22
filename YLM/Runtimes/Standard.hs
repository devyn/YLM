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
import Data.Foldable (foldlM, foldrM)
import Control.Monad.Trans
import Data.List
import Control.Applicative

instance Runtime Standard where
  -- Apply
  evaluate (Standard m) (Cons (Label l) b)                = maybe (Right $ Cons (Label l) b) (flip eApply (m,b))
                                                                  (Map.lookup l m)
  evaluate (Standard m) (Cons fn@(Cons (Label "->") x) d) = sapl fn (m,d)
  evaluate (Standard m) (Cons a b)                        = do x <- evaluate (Standard m) a
                                                               y <- evaluate (Standard m) b
                                                               evaluate (Standard m) (Cons x y)
  evaluate (Standard m) (Label l)                         = maybe (Right $ Label l) eSerialize (Map.lookup l m)
  evaluate (Standard m) x                                 = Right x
  
  execute (Standard m) (i:es) = f (Standard m) i
                                >>= either (return.Left) (\ (r', e') ->
                                                           if null es
                                                           then return $ Right (r', e')
                                                           else do execute r' es >>= either (return.Left) (\ (r'', e'') ->
                                                                                                   return $ Right (r'', e'')))
    where f r@(Standard m) (Cons (Label l) b) = maybe (return (Right (r, Cons (Label l) b)))
                                                      (\ e -> eExecute e (m, b) >>= either (return.Left) (\ (m', b') ->
                                                                                                           return $ Right (Standard m', b')))
                                                      (Map.lookup l m)
          f r@(Standard m) x                  = flip (either $ return . Left) (evaluate r x) (\ b' ->
                                                                                               return $ Right (r, b'))
  execute (Standard m) []                     = return $ Right (Standard m, Nil)

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
                                                       , eExecute   = wrapE $ aError })
                           ,("def",      StandardEntry { eSerialize = szNat "def"
                                                       , eApply     = aDef
                                                       , eExecute   = eDef })
                           ,("undef",    StandardEntry { eSerialize = szNat "undef"
                                                       , eApply     = aUndef
                                                       , eExecute   = eUndef })
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
                                                       , eApply     = (\(m,x)->Right $ Cons (Label "->") x)
                                                       , eExecute   = wrapE $ (\(m,x)->Right $ Cons (Label "->") x) })
                           ,("true",     StandardEntry { eSerialize = sTrue
                                                       , eApply     = sapl  (either (const Nil) id sTrue)
                                                       , eExecute   = sexec (either (const Nil) id sTrue) })
                           ,("false",    StandardEntry { eSerialize = sFalse
                                                       , eApply     = sapl  (either (const Nil) id sFalse)
                                                       , eExecute   = sexec (either (const Nil) id sFalse) })
                           ,("read",     StandardEntry { eSerialize = szNat "read"
                                                       , eApply     = aRead
                                                       , eExecute   = wrapE $ aRead })
                           ,("write",    StandardEntry { eSerialize = szNat "write"
                                                       , eApply     = aWrite
                                                       , eExecute   = wrapE $ aWrite })
                           ,("bind",     StandardEntry { eSerialize = szNat "bind"
                                                       , eApply     = aBind
                                                       , eExecute   = eBind })
                           ,("id",       StandardEntry { eSerialize = sId
                                                       , eApply     = sapl  (either (const Nil) id sId)
                                                       , eExecute   = sexec (either (const Nil) id sId) })
                           ,("list",     StandardEntry { eSerialize = szNat "list" {- may be serializable in the future -}
                                                       , eApply     = aList
                                                       , eExecute   = wrapE $ aList })
                           ,("cons",     StandardEntry { eSerialize = szNat "cons"
                                                       , eApply     = aCons
                                                       , eExecute   = wrapE $ aCons })
                           ,("head",     StandardEntry { eSerialize = szNat "head"
                                                       , eApply     = aHead
                                                       , eExecute   = wrapE $ aHead })
                           ,("tail",     StandardEntry { eSerialize = szNat "tail"
                                                       , eApply     = aTail
                                                       , eExecute   = wrapE $ aTail })
                           ,("=",        StandardEntry { eSerialize = szNat "="
                                                       , eApply     = aEq
                                                       , eExecute   = wrapE $ aEq })]

aPutLine (m, x) = Right $ Cons (Label "put-line") x

ePutLine (m, x@(Cons _ _))  = either (return . Left) (\ a -> f (m, a)) (mapM (evaluate (Standard m)) $ ctl x)
  where f (m, (Label x):[]) = do putStrLn x
                                 return $ Right (m, Nil)
        f (m, (Label x):d)  = do putStrLn x
                                 f (m, d)
        f (m, [])           = return $ Right (m, Nil)
        f (m, b)            = return $ Left "type mismatch (expected: label...)"
ePutLine (m, Nil)           = return $ Right (m, Nil)
ePutLine (m, b)             = return $ Left "type mismatch (expected: label...)"

aGetLine (m, x) = Right $ Cons (Label "get-line") x

eGetLine (m, _) = do l <- getLine
                     return $ Right (m, Label l)

aQuote (m, (Cons x Nil)) = Right x
aQuote (m, _)            = Left "type mismatch (expected: any)"

aError (m, Cons (Label l) Nil) = Left l
aError (m, _)                  = Left "type mismatch (expected: label)"

aDef (m, x) = Right $ Cons (Label "def") x

eDef (m, Cons (Label l) (Cons x Nil)) = return $ Right (Map.insert l (sdefent m x) m, Label l)

aUndef (m, x) = Right $ Cons (Label "undef") x

eUndef (m, Cons (Label l) Nil) = return $ Right (Map.delete l m, Label l)

aAdd                = mathematical (+) (+)    (NumInt 0)

aSub (m, Cons c d)  = mathematical (-) (-) c (m, d)
aSub (m, Nil)       = Right $ NumInt 0

aMul                = mathematical (*) (*)    (NumInt 1)

aDiv (m, Cons c d)  = mathematical (/) (quot) c (m, d)
aDiv (m, Nil)       = Right $ NumInt 1

aExp (m,(Cons c d)) = mathematical (**) (^) c (m,d)
aExp (m,Nil)        = Right $ NumInt 1

sTrue  = Right $ ltc [Label "->", ltc [Label "a", Label "b"], Label "a"]

sFalse = Right $ ltc [Label "->", ltc [Label "a", Label "b"], Label "b"]

aRead (m, (Cons (Label l) Nil)) =
  case ylmRead (Standard m) l of
    Left err     -> Left err
    Right []     -> Right Nil
    Right (x:[]) -> Right x
    Right xs     -> Right $ ltc xs
aRead (m, (Cons b Nil)) =
  case evaluate (Standard m) b of
    Right (Label l) -> aRead (m, (Cons (Label l) Nil))
    Right _         -> Left "type mismatch (expected: label)"
    Left  err       -> Left err
aRead (m, _)         = Left "type mismatch (expected: label)"

aWrite (m, xs) =
  do xs' <- mapM (evaluate (Standard m)) $ ctl xs
     case ylmWrite (Standard m) xs' of
       Left err -> Left err
       Right s  -> Right $ Label s

aBind (m, x) = Right $ Cons (Label "bind") x

eBind (m, (Cons (Label v) (Cons e xs))) =
  do va <- (execute (Standard m) [e]) 
     flip (either $ return . Left) va $ \ (Standard nm, r) ->
       do vb <- execute (Standard (Map.insert v (sdefent nm r) nm)) $ ctl xs
          flip (either $ return . Left) vb $ \ (Standard m', x') ->
            return $ Right (m, x')

sId = Right $ ltc [Label "->", ltc [Label "x"], Label "x"]

aList (m, x) = case find cond lt of
                 Just e  -> e
                 Nothing -> Right $ ltc $ map (either undefined id) lt
  where lt = map (evaluate (Standard m)) (ctl x)
        cond x = case x of { (Left _) -> True; (Right _) -> False }

aCons (m, (Cons x (Cons l Nil))) = case evaluate (Standard m) x of
                                     Left  err -> Left err
                                     Right v   -> case evaluate (Standard m) l of
                                                    Left  err -> Left err
                                                    Right n   -> Right $ Cons v n
aCons (m, _) = Left "expected 2 arguments"

aHead (m, (Cons l Nil)) = case evaluate (Standard m) l of
                 Right (Cons x _) -> Right x
                 Right Nil        -> Left  "empty list"
                 Right _          -> Left  "type mismatch (expected: list)"
                 Left  err        -> Left  err
aHead (m, _) = Left "expected 1 argument of type list"

aTail (m, (Cons l Nil)) = case evaluate (Standard m) l of
                 Right (Cons _ y) -> Right y
                 Right Nil        -> Left  "empty list"
                 Right _          -> Left  "type mismatch (expected: list)"
                 Left  err        -> Left  err
aTail (m, _) = Left "expected 1 argument of type list"

aEq :: (Map String StandardEntry, Elem) -> Either String Elem

aEq (m, l@(Cons _ (Cons _ _))) = Right $ tBool (all (\ (a, b) -> a == b) ((,) <$> ev <*> ev))
  where ev = map t (filter c (map (evaluate (Standard m)) $ ctl l))
        t (Right x) = x
        c (Right x) = True
        c (Left  x) = False
        tBool True  = either (const Nil) id sTrue
        tBool False = either (const Nil) id sFalse
aEq (m, l@(Cons _ Nil))        = sTrue
aEq (m, Nil)                   = sFalse
aEq (m, _)                     = Left "expected arguments"

szNat s = Right $ Cons (Label "native") (Cons (Label s) Nil)

wrapE f (m, x) = either (return . Left) (\ a -> return $ Right (m, a)) $ f (m, x)

sfun :: [Elem] -> Elem -> (Map String StandardEntry, Elem) -> Either String Elem

sfun args body (m, x) = if length args == length ia
                          then evaluate (Standard $ Map.union bv m) body
                          else Left ("expected " ++ show (length args)
                                                           ++ " argument(s), got " ++ show (length ia))
  where ia = ctl x
        bv = Map.fromList $ zip (map (\ (Label l) -> l) args) (map (sdefent m) ia)

sdefent :: Map String StandardEntry -> Elem -> StandardEntry

sdefent m x = StandardEntry { eSerialize = lx
                            , eApply     = \ t -> either Left (\ d -> sapl d t) lx
                            , eExecute   = \ t -> either (return . Left) (\ d -> sexec d t) lx }
  where lx = evaluate (Standard m) x

sapl :: Elem -> (Map String StandardEntry, Elem) -> Either String Elem

sapl fb@(Cons (Label "->")
              (Cons ar (Cons fn Nil))) (m,x) = sfun (ctl ar) fn (m,x)
sapl b (m,x)  = Left $ "type mismatch: (" ++
                          either (const "!write-error") id (ylmWrite standardLib [b]) ++
                          " is not a callable form)"

sexec :: Elem -> (Map String StandardEntry, Elem) -> IO (Either String (Map String StandardEntry, Elem))

sexec b (m,a) = return $ either Left (\ v -> Right (m,v)) (sapl b (m,a))

ctl Nil = []
ctl (Cons h t) = h : ctl t

ltc [] = Nil
ltc (h:t) = Cons h (ltc t)

mathematical :: (Double  -> Double  -> Double )
             -> (Integer -> Integer -> Integer)
             -> Elem
             -> (Map String StandardEntry, Elem)
             -> Either String Elem

mathematical ff fi ss (m,a) = either Left (\ s -> foldlM f s (ctl a)) (evaluate (Standard m) ss)
  where f s e = let ee = evaluate (Standard m) e
                in case (s,ee) of
                  (NumInt   si, Right (NumInt   ni)) -> Right $ NumInt   $ si `fi` ni
                  (NumInt   si, Right (NumFloat nf)) -> Right $ NumFloat $ fromIntegral si `ff` nf
                  (NumFloat sf, Right (NumInt   ni)) -> Right $ NumFloat $ sf `ff` fromIntegral ni
                  (NumFloat sf, Right (NumFloat nf)) -> Right $ NumFloat $ sf `ff` nf
                  (_, Right _)                       -> Left "type mismatch (expected: int or float)"
                  (_, Left  err)                     -> Left err

mathematicalr :: (Double  -> Double  -> Double )
              -> (Integer -> Integer -> Integer)
              -> Elem
              -> (Map String StandardEntry, Elem)
              -> Either String Elem

mathematicalr ff fi ss (m,a) = either Left (\ s -> foldrM f s (ctl a)) (evaluate (Standard m) ss)
  where f s e = let ee = evaluate (Standard m) e
                in case (s,ee) of
                  (NumInt   si, Right (NumInt   ni)) -> Right $ NumInt   $ si `fi` ni
                  (NumInt   si, Right (NumFloat nf)) -> Right $ NumFloat $ fromIntegral si `ff` nf
                  (NumFloat sf, Right (NumInt   ni)) -> Right $ NumFloat $ sf `ff` fromIntegral ni
                  (NumFloat sf, Right (NumFloat nf)) -> Right $ NumFloat $ sf `ff` nf
                  (_, Right _)                       -> Left "type mismatch (expected: int or float)"
                  (_, Left  err)                     -> Left err