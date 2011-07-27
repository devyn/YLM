module YLM.Runtime (
  yeval,
  yexec,
  yrun
) where

import YLM.Data
import YLM.Data.Util
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldlM, foldrM)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as S

yeval :: Scope              -- ^ The scope to evaluate within.
      -> Elem               -- ^ The input to evaluate.
      -> Either String Elem -- ^ Either an error or the return value.

yeval m (Cons (NumInt _) y)                 = err ["can't call a number!"]
yeval m (Cons (NumFloat _) y)               = err ["can't call a number!"]
yeval m (Cons (Lambda s as b) y)            = do c <- bindArgs m s as y
                                                 yeval c b
yeval m (Cons (Opaque _ f) y)               = f m y
yeval m (Cons a y)                          = do x <- yeval m a
                                                 yeval m (Cons x y)
yeval m (Label l)                           = maybe (err [l, " is not defined."])
                                                    Right (Map.lookup l m)
yeval m (Action _)                          = err ["can't evaluate an impure action from pure code!"]
yeval m x                                   = Right x

yexec :: Scope                            -- ^ The scope to evaluate within.
      -> [Elem]                           -- ^ The input to execute.
      -> IO (Either String (Scope, Elem)) -- ^ The IO side-effects, and either an error or the changed state and returned value.

yexec m ((Action a):es) = do ia <- a
                             (flip (either (return . Left))) ia $ \ (m', _) ->
                               if null es
                                  then return ia
                                  else yexec m' es

yexec m (e:es) = do let e' = yeval m e
                    case e' of
                      Right (Action a) -> do
                        ia <- a
                        (flip (either (return . Left))) ia $ \ (m', _) ->
                          if null es
                             then return ia
                             else yexec m' es
                      Right r ->
                        if null es
                           then return $ Right (m, r)
                           else yexec m es
                      Left err ->
                        return $ Left err

yexec m [] = return $ Right (m, Nil)

yrun :: (TextInterface i)
     => i                                -- ^ Text interface to use
     -> Scope                            -- ^ Initial scope to use
     -> String                           -- ^ File name (for parse errors)
     -> String                           -- ^ Input
     -> IO (Either String (Scope, Elem)) -- ^ IO-wrapped either an error or the result and changed state.

yrun ifc s fn si = either (return . Left) (yexec s) $ yread ifc fn si

bindArgs :: Scope -> Scope -> [Argument] -> Elem -> Either String Scope

bindArgs m s ad ai = do eai <- mev m ai
                        bns <- fov (Map.empty) 0 ad eai
                        return $ Map.union bns s
  where mev s (Cons x ys) = do ex <- yeval s x
                               either Left (Right . Cons ex) $ mev s ys
        mev s x           = yeval s x
        fov s _ ((Rest n):_) e =
          Right $ Map.insert n e s
        fov s c (a:as) (Cons x ys) =
          case a of
            Required n -> fov (Map.insert n x s) (c+1) as ys
            Optional n -> fov (Map.insert n x s) (c+1) as ys
        fov s c [] Nil =
          Right s
        fov _ c as Nil =
          err ["not enough arguments (expected ", showArity $ arity ad, " argument(s), got ", show c, ")."]
        fov _ c [] l
          | isPureList l = err ["too many arguments (expected ", showArity $ arity ad
                               ," argument(s), got ", show $ c + sizeCons l, ")."]
          | otherwise    = err ["can't cons this type of argument to this function."]
        fov _ _ _ _ =
          err ["unexpected error while binding arguments."]