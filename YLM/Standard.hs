module YLM.Standard (standard) where

import YLM.Data
import YLM.Data.Util
import YLM.Runtime
import YLM.Interfaces.Raw
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldlM, foldrM)
import Data.List
import Control.Monad (filterM)
import Control.Applicative
import System.Plugins.Load
import System.Directory
import System.FilePath
import System.Environment
import System.IO

-- TODO: set-scope; merge-window; concat; delete-file; stat-file;
--       load; empty; read-from-file; do-with; list-directory; apply

standard =
  Map.fromList [o "read"          oRead
               ,o "write"         oWrite
               ,o "pretty-print"  oPrettyPrint
               ,o "load"          oLoad
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
               ,o "map"           oMap
               ,o "filter"        oFilter
               ,o "left-fold"     oLeftFold
               ,o "right-fold"    oRightFold
               ,o "explode"       oExplode
               ,o "implode"       oImplode
               ,o "do"            oDo
               ,o "bind"          oBind
               ,o "put-line"      oPutLine
               ,o "get-line"      oGetLine
               ,o "read-file"     oReadFile
               ,o "write-file"    oWriteFile
               ,o "append-file"   oAppendFile
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

oLoad s (Cons mn Nil) =
  do mn' <- yeval s mn
     case mn' of
       Label mod -> Right $ Action $ \ m ->
         do mfn <- hFindModule s mod
            case mfn of
              ModuleNotFound ->
                return $ Left $ concat ["could not find any loadable files matching `", mod
                                       ,"'. please ensure that it is on your load-path."]
              InvalidLoadPath ->
                return $ Left $ concat ["your load-path variable is set to something that (load)"
                                        ," can not understand. it should be defined as a list of" 
                                        ," directories for (load) to search."]
              LoadPathNotDefined ->
                return $ Left $ concat ["your load-path variable is not set. it should be defined"
                                       ," as a list of directories for (load) to search."]
              FindModuleError e ->
                return $ Left e
              YSTModuleFile path ->
                flip catch (return . Left . show) $
                  do f <- readFile path
                     res <- yrun Raw standard path f
                     either (return . Left) (return . Right . (\x->(m,x)) . Window . fst) res
              CompiledModuleFile path ->
                flip catch (return . Left . show) $
                  do let loadPath = map (\ (Label x) -> x) $ clist $
                                      maybe undefined
                                            (either undefined id)
                                            (Map.lookup "load-path" s)
                     ylmLibPath <- getEnv "YLM_OBJECT_PATH"
                     -- Warning: Loading Haskell modules is potentially unsafe at the moment.
                     -- If ystModule is something other than IO Scope, some seriously weird
                     -- corruption and/or crashes are likely to occur.
                     ms <- load path (ylmLibPath : loadPath) [] "ystModule"
                     case ms of
                       LoadSuccess _ ios -> do
                         scope <- ios
                         return $ Right $ (m, Window scope)
                       LoadFailure errors ->
                         return $ Left $ intercalate "\n " errors
       _ -> tpe "label" mn'
oLoad s x = fallback "1" x

data FindModuleResult = ModuleNotFound
                      | InvalidLoadPath
                      | LoadPathNotDefined
                      | FindModuleError String
                      | YSTModuleFile String
                      | CompiledModuleFile String

hFindModule s modname
  | isAbsolute modname =
    do isE <- doesFileExist modname
       return $ if isE
                   then case takeExtension modname of
                          ".o"   -> CompiledModuleFile modname
                          _      -> YSTModuleFile      modname
                   else ModuleNotFound
  | otherwise = flip catch (return . FindModuleError . show) $ do
    case Map.lookup "load-path" s of
      Just (Right lpv)
        | isPureList lpv && all ((== "label") . ytype) (clist lpv) ->
          do let loadPath = map (\ (Label x) -> x) $ clist lpv
             let tryMatch baseName = mapM (doesFileExist . (</> baseName)) loadPath
                                     >>= return . fmap ((</> baseName) . fst) . find snd . zip loadPath
             cm <- tryMatch (modname <.> ".o")
             ym <- tryMatch (modname <.> ".yst")
             om <- tryMatch modname
             return $ maybe ModuleNotFound id (CompiledModuleFile <$> cm <|>
                                               YSTModuleFile      <$> ym <|>
                                               YSTModuleFile      <$> om)
        | otherwise ->
            return InvalidLoadPath
      _ ->
        return LoadPathNotDefined

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
  where subf l = if null l then 0 else foldl (-) (head l) (tail l)
        subi l = if null l then 0 else foldl (-) (head l) (tail l)

oMultiply = hMath product product

oDivide = hMath divf divi
  where divf l = if null l then 1 else foldl (/)    (head l) (tail l)
        divi l = if null l then 1 else foldl (quot) (head l) (tail l)

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

oNullQ s (Cons l Nil) =
  do l' <- yeval s l
     case l' of
       Nil      -> Right tTrue
       Cons _ _ -> Right tFalse
       _        -> tpe "list" l'
oNullQ s x = fallback "1" x

oMap s (Cons fun (Cons l Nil)) =
  do fun' <- yeval s fun
     l'   <- yeval s l
     if isPureList l'
        then do t <- flip mapM (clist l') $ \ x ->
                       yeval s $ lcons [fun, lcons [Label "quote", x]]
                Right $ lcons t
        else err ["can't map over a dirty (assoc) list."]
oMap s x = fallback "2" x

oFilter s (Cons fun (Cons l Nil)) =
  do fun' <- yeval s fun
     l'   <- yeval s l
     if isPureList l'
        then do t <- flip filterM (clist l') $ \ x ->
                       do fr <- yeval s $ lcons [fun, q x]
                          tr <- yeval s $ lcons [fr,  q (Label "yes"),
                                                      q (Label "no")]
                          case tr of
                            Label "yes" -> Right True
                            Label "no"  -> Right False
                            _           -> err ["the filtering function did not return a boolean function."]
                Right $ lcons t
        else err ["can't filter a dirty (assoc) list."]
  where q v = lcons [Label "quote", v]
oFilter s x = fallback "2" x

oLeftFold = hFold foldlM

oRightFold = hFold foldrM

hFold :: ((Elem -> Elem -> Either String Elem) -> Elem -> [Elem] -> Either String Elem) -> Scope -> Elem -> Either String Elem

hFold ff s (Cons fun (Cons inv (Cons l Nil))) =
  do fun' <- yeval s fun
     inv' <- yeval s inv
     l'   <- yeval s l
     if isPureList l'
        then fl inv' (clist l') $ \ y x ->
               yeval s $ lcons [fun, q y, q x]
        else err ["can't fold over a dirty (assoc) list."]
  where fl b c a = ff a b c
        q v      = lcons [Label "quote", v]
hFold ff s x = fallback "3" x

oExplode s (Cons lb Nil) =
  do lb' <- yeval s lb
     case lb' of
       Label l ->
         Right $ lcons $ map (\ c -> Label [c]) l
       _ -> tpe "label" lb'
oExplode s x = fallback "1" x

oImplode s (Cons ul Nil) =
  do l <- yeval s ul
     if isPureList l
        then do m <- flip mapM (clist l) $
                       \ x -> case x of
                                Label lb -> Right lb
                                _        -> tpe "label" x
                Right $ Label $ concat m
        else tpe "list" l
oImplode s x = fallback "1" x

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

oReadFile s (Cons fnm Nil) =
  do fnm' <- yeval s fnm
     case fnm' of
       Label fname -> Right $ Action $ \ m ->
         flip catch (return . Left . show) $
           do c <- readFile fname
              return $ Right (m, Label c)
       _ -> tpe "label" fnm'
oReadFile s x = fallback "1" x

oWriteFile s (Cons fnm (Cons cts Nil)) =
  do fnm' <- yeval s fnm
     cts' <- yeval s cts
     case (fnm', cts') of
       (Label fname, Label contents) ->
         Right $ Action $ \ m ->
           flip catch (return . Left . show) $
             do writeFile fname contents
                return $ Right (m, lcons [Label "wrote"
                                         ,NumInt $ fromIntegral (length contents), Label "bytes"])
       _ -> tpem "labels" [fnm', cts']

oAppendFile s (Cons fnm (Cons cts Nil)) =
  do fnm' <- yeval s fnm
     cts' <- yeval s cts
     case (fnm', cts') of
       (Label fname, Label contents) ->
         Right $ Action $ \ m ->
           flip catch (return . Left . show) $
             do appendFile fname contents
                return $ Right (m, lcons [Label "appended"
                                         ,NumInt $ fromIntegral (length contents), Label "bytes"])

tXyzzy = Action $ \ m -> do putStrLn "\ESC[34m ~ Nothing happens.\ESC[0m"
                            return $ Right (m, Nil)


wcle = Left "I'm afraid I can't let you do that, for fear of the formation of a black hole."