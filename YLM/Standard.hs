module YLM.Standard (standard) where

import YLM.Data
import YLM.Data.Util
import YLM.Runtime
import Data.Map (Map)
import qualified Data.Map as Map

-- TODO: put-line; get-line; error; def; undef; +; -; *; /; ^; read; write; bind; let; list; cons; head; tail; =; do; load

standard =
  Map.fromList [o "->"            oLambda
               ,o "quote"         oQuote
               ,t "id"            tId
               ,t "true"          tTrue
               ,t "false"         tFalse]
  where o n f = (n, Opaque n f)
        t n v = (n, v)

oLambda s (Cons as (Cons b Nil)) =
  do al <- argList as
     return $ Lambda s al b
oLambda s _ =
  err ["malformed lambda expression."]

oQuote s (Action _)   = err ["can't quote an impure action!"]
oQuote s (Cons x Nil) = Right x
oQuote s x            = fallback "1" x

tId = Lambda Map.empty [Required "a"] (Label "a")

tTrue = Lambda Map.empty [Required "a", Required "b"] (Label "a")

tFalse = Lambda Map.empty [Required "a", Required "b"] (Label "b")