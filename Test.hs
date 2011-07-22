import Test.HUnit hiding (Label)

import YLM.Elem
import YLM.TextInterface
import YLM.Runtime
import YLM.Interfaces.Standard
import YLM.Runtimes.Standard
import Data.Map (Map)
import qualified Data.Map as Map

elemReadEmptyStringZero =
  TestCase $ assertEqual "Reading an empty string returns an empty list."
               (Right [])
               (ylmRead (Standard Map.empty) "")

elemReadSingleLabel =
  TestCase $ assertEqual "Reading a single label returns a single Label."
               (Right [Label "hello"])
               (ylmRead (Standard Map.empty) "hello")

elemReadMultiLists =
  TestCase $ assertEqual "Reading () (a) (a b) returns appropriate values."
               (Right [Nil
                      , Cons (Label "a") Nil
                      , Cons (Label "a") (Cons (Label "b") Nil)])
               (ylmRead (Standard Map.empty) "() (a) (a b)")

elemReadAssocList =
  TestCase $ assertEqual "Reader can handle association lists."
               (Right [Cons (Cons (Label "a") (Label "b"))
                            (Cons (Cons (Label "c") (Label "d")) Nil)])
               (ylmRead (Standard Map.empty) "((a . b) (c . d))")

elemReadQuote =
  TestCase $ assertEqual "Reader properly expands the (quote) form for a quoted input."
               (Right [Cons (Label "quote")
                            (Cons (Label "a") Nil)
                      ,Cons (Label "quote")
                            (Cons (Cons (Label "a") (Cons (Label "b") Nil)) Nil)])
               (ylmRead (Standard Map.empty) "'a '(a b)")

elemReadQuotedQuote =
  TestCase $ assertEqual "Reader properly expands ''a as (quote (quote a))."
               (Right [Cons (Label "quote")
                            (Cons (Cons (Label "quote") (Cons (Label "a") Nil)) Nil)])
               (ylmRead (Standard Map.empty) "''a")

elemReadMathExpr =
  TestCase $ assertEqual "Reader properly parses mathematical expressions including numbers."
               (Right [Cons (Label "+")
                            (Cons (NumInt 1)
                                  (Cons (NumFloat 2.0)
                                        (Cons (Cons (Label "-")
                                                    (Cons (NumInt 6)
                                                          (Cons (NumInt 1) Nil))) Nil)))])
               (ylmRead (Standard Map.empty) "(+ 1 2.0 (- 6 1))")

elemReadEscapeCode =
  TestCase $ assertEqual "Reader properly recognizes escape codes in long form labels."
               (Right [Label "hello world \"\r\n\" \0 \a \b \f \t \v \ESC"])
               (ylmRead (Standard Map.empty)
                "\"hello world \\\"\\r\\n\\\" \\0 \\a \\b \\f \\t \\v \\e\"")

elemReadWhitespace =
  TestCase $ assertEqual "Reader properly ignores whitespace including newlines."
               (Right [Cons (Label "a") (Cons (Label "b") (Cons (Label "c") Nil))])
               (ylmRead (Standard Map.empty)
                "\t( a\n\tb\r\nc\n)\t\n")

elemReadLookupFunction =
  TestCase $ assertEqual "Reader properly reads definition for the (lookup) function."
               (Right [Cons (Label "def") (Cons (Label "lookup") (Cons (Cons (Label "->") (Cons (Cons (Label "table") (Cons (Label "name") Nil)) (Cons (Cons (Cons (Label "=") (Cons (Label "table") (Cons (Cons (Label "quote") (Cons Nil Nil)) Nil))) (Cons (Cons (Label "error") (Cons (Label "not found") Nil)) (Cons (Cons (Cons (Label "=") (Cons (Cons (Label "head") (Cons (Cons (Label "head") (Cons (Label "table") Nil)) Nil)) (Cons (Label "name") Nil))) (Cons (Cons (Label "tail") (Cons (Cons (Label "head") (Cons (Label "table") Nil)) Nil)) (Cons (Cons (Label "lookup") (Cons (Cons (Label "tail") (Cons (Label "table") Nil)) (Cons (Label "name") Nil))) Nil))) Nil))) Nil))) Nil))])
               (ylmRead (Standard Map.empty)
                "(def lookup (-> (table name) ((= table '()) (error \"not found\") ((= (head (head table)) name) (tail (head table)) (lookup (tail table) name)))))")

readerTests =
  TestLabel "Standard Reader" $
    TestList [elemReadEmptyStringZero
             ,elemReadSingleLabel
             ,elemReadMultiLists
             ,elemReadAssocList
             ,elemReadQuote
             ,elemReadQuotedQuote
             ,elemReadMathExpr
             ,elemReadEscapeCode
             ,elemReadWhitespace
             ,elemReadLookupFunction]

evalIdentity =
  TestCase $ assertEqual "Evaluates identity function (id 1) => 1."
               (Right $ NumInt 1)
               (ylmRead standardLib "(id 1)" >>= evaluate standardLib . head)

evalLambda =
  TestCase $ assertEqual "Evaluates lambda expression (-> (a b) a) properly."
               (Right $ Label "yes")
               (ylmRead standardLib "((-> (a b) a) 'yes 'no)" >>= evaluate standardLib . head)

evalLabel =
  TestCase $ assertEqual "Returns serialization when given a defined label."
               (ylmRead standardLib "(-> (x) x)" >>= return . head)
               (ylmRead standardLib "id" >>= evaluate standardLib . head)

evalGenCons =
  TestCase $ assertEqual "Applies list expanded as arguments when cons'd in."
               (ylmRead standardLib "(yes yes)" >>= return . head)
               (ylmRead standardLib "((-> (a b) '(yes yes)) . '(ha ha))"
                >>= evaluate standardLib . head)

evalUnknownNoTouch =
  TestCase $ assertEqual "Evaluator won't touch argument if it doesn't know what it is."
               (ylmRead standardLib "(haha haha haha)" >>= return . head)
               (ylmRead standardLib "(haha haha haha)" >>= evaluate standardLib . head)

evalIOReturnsIOAction =
  TestCase $ assertEqual "Evaluator returns the called function sexp if the function uses IO."
               (ylmRead standardLib "(bind line (get-line) (put-line line))" >>= return . head)
               (ylmRead standardLib "(bind line (get-line) (put-line line))" >>= evaluate standardLib . head)

evalQuoteReturnsQuotedElem =
  TestCase $ assertEqual "Evaluator returns (quote a) => a."
               (ylmRead standardLib "a" >>= return . head)
               (ylmRead standardLib "'a" >>= evaluate standardLib . head)

evalErrorReturnsLeft =
  TestCase $ assertEqual "Evaluating an (error) construct returns Left err."
               (Left "oops")
               (ylmRead standardLib "(error oops)" >>= evaluate standardLib . head)

evalRefusesDefineGlobal =
  TestCase $ assertEqual "Evaluator refuses to globally define a name-value pair."
               (ylmRead standardLib "(def foo 'bar)" >>= return . head)
               (ylmRead standardLib "(def foo 'bar)" >>= evaluate standardLib . head)

evalRefusesUndefGlobal =
  TestCase $ assertEqual "Evaluator refuses to globally undefine a name."
               (ylmRead standardLib "(undef foo)" >>= return . head)
               (ylmRead standardLib "(undef foo)" >>= evaluate standardLib . head)

evalMathAdd =
  TestCase $ assertEqual "Evaluator can add numbers."
               (Right $ NumInt 17)
               (ylmRead standardLib "(+ 4 2 3 1 2 5)" >>= evaluate standardLib . head)

evalMathSub =
  TestCase $ assertEqual "Evaluator can subtract numbers."
               (Right $ NumInt 4)
               (ylmRead standardLib "(- 10 2 4)" >>= evaluate standardLib . head)

evalMathMul =
  TestCase $ assertEqual "Evaluator can multiply numbers."
               (Right $ NumFloat 2.4)
               (ylmRead standardLib "(* 0.6 2 2)" >>= evaluate standardLib . head)

evalMathDiv =
  TestCase $ assertEqual "Evaluator can divide numbers."
               (Right $ NumFloat (10/3.0))
               (ylmRead standardLib "(/ 100 10 3.0)" >>= evaluate standardLib . head)

evalMathIntDiv =
  TestCase $ assertEqual "Evaluator supports integer division."
               (Right $ NumInt 0)
               (ylmRead standardLib "(/ 1 2)" >>= evaluate standardLib . head)

evalMathExp =
  TestCase $ assertEqual "Evaluator supports exponentiation."
               (Right $ NumInt 256)
               (ylmRead standardLib "(^ 2 8)" >>= evaluate standardLib . head)

evalMathExpFrac =
  TestCase $ assertEqual "Evaluator supports raising numbers to fractional exponents."
               (Right $ NumFloat 10.0)
               (ylmRead standardLib "(^ 100 (/ 1 2.0))" >>= evaluate standardLib . head)

evalMathQuadratic =
  TestCase $ assertEqual "Evaluator can solve the quadratic formula."
               (Right $ NumFloat 1.0)
               (ylmRead standardLib "(/ (+ (* -1 3) (^ (- (^ 3 2) (* 4 1 -4)) 0.5)) (* 2 1))" >>= evaluate standardLib . head)

evalBoolean =
  TestCase $ assertEqual "(true) and (false) are Church booleans."
               (Right $ Cons (Label "yes") (Cons (Label "no") Nil))
               (ylmRead standardLib "(list (true 'yes 'no) (false 'yes 'no))" >>= evaluate standardLib . head)

evalReader =
  TestCase $ assertEqual "Evaluator supports (read)."
               (Right $ Cons (Label "a") (Cons (Label "b") Nil))
               (ylmRead standardLib "(read \"(a b)\")" >>= evaluate standardLib . head)

evalWriter =
  TestCase $ assertEqual "Evaluator supports (write)."
               (Right $ Label "(a b)")
               (ylmRead standardLib "(write '(a b))" >>= evaluate standardLib . head)

evalCons =
  TestCase $ assertEqual "Evaluator supports (cons)."
               (Right $ (Cons (Label "a") (Label "b")))
               (ylmRead standardLib "(cons 'a 'b)" >>= evaluate standardLib . head)

evalHead =
  TestCase $ assertEqual "Evaluator supports (head)."
               (Right $ Label "a")
               (ylmRead standardLib "(head '(a . b))" >>= evaluate standardLib . head)

evalTail =
  TestCase $ assertEqual "Evaluator supports (tail)."
               (Right $ Label "b")
               (ylmRead standardLib "(tail '(a . b))" >>= evaluate standardLib . head)

evalEquivalence =
  TestCase $ assertEqual "Evaluator can check for equivalence with (=), and returns a Church boolean lambda as its result."
               (Right $ Cons (Label "yes") (Cons (Label "no") Nil))
               (ylmRead standardLib "(list ((= 132 132 132) 'yes 'no) ((= 132 123 'a) 'yes 'no))"
                >>= evaluate standardLib . head)

evalLetOne =
  TestCase $ assertEqual "Evaluator can bind a name to a pure value within an expression using (let)."
               (Right $ NumInt 4)
               (ylmRead standardLib "(let (n . (* 1 3)) (+ n 1))" >>= evaluate standardLib . head)

evalLetMultiple =
  TestCase $ assertEqual "Evaluator can bind multiple names to multiple pure values in an expression using (let)."
               (Right $ NumInt 10)
               (ylmRead standardLib "(let ((n1 . (* 2 2)) (n2 . 6)) (+ n1 n2))"
                >>= evaluate standardLib . head)

evaluatorTests =
  TestLabel "Standard Evaluator" $
    TestList [evalIdentity
             ,evalLambda
             ,evalLabel
             ,evalGenCons
             ,evalUnknownNoTouch
             ,evalIOReturnsIOAction
             ,evalQuoteReturnsQuotedElem
             ,evalErrorReturnsLeft
             ,evalRefusesDefineGlobal
             ,evalRefusesUndefGlobal
             ,evalMathAdd
             ,evalMathSub
             ,evalMathMul
             ,evalMathDiv
             ,evalMathIntDiv
             ,evalMathExp
             ,evalMathExpFrac
             ,evalMathQuadratic
             ,evalBoolean
             ,evalReader
             ,evalWriter
             ,evalCons
             ,evalHead
             ,evalTail
             ,evalEquivalence
             ,evalLetOne
             ,evalLetMultiple]

execFallback =
  TestCase $ do v <- run standardLib "(+ 1 2)"
                (flip . either) (const $ assertFailure n) v $ \ (Standard m,a) ->
                  assertEqual n a (NumInt 3)
  where n = "Executor falls back on evaluator for pure functions."

execDefine =
  TestCase $ do v <- run standardLib "(def foo 'bar)"
                (flip . either) (const $ assertFailure n) v $ \ (Standard m,a) ->
                  assertBool n (Map.member "foo" m)
  where n = "Executor is able to define global names."

execUndef =
  TestCase $ do v <- run standardLib "(undef put-line)"
                (flip . either) (const $ assertFailure n) v $ \ (Standard m,a) ->
                  assertBool n (not $ Map.member "put-line" m)
  where n = "Executor is able to undefine global names, even built-ins."

-- TODO: use an IO function
execBind =
  TestCase $ do v <- run standardLib "(bind res (+ 2 3)) (- res 1)"
                (flip . either) (const $ assertFailure n) v $ \ (Standard m,a) ->
                  assertEqual n a (NumInt 4)
  where n = "(bind) is able to bind a name to an IO value."

execMultipleStatements =
  TestCase $ do v <- run standardLib "(+ 1 2) (+ 3 4)"
                (flip . either) (const $ assertFailure n) v $ \ (Standard m,a) ->
                  assertEqual n a (NumInt 7)
  where n = "Executing multiple statements returns the value of the last statement."

executorTests =
  TestLabel "Standard Executor (impure functions)" $
    TestList [execFallback
             ,execDefine
             ,execUndef
             ,execBind
             ,execMultipleStatements]

main =
  do v <- runTestTT $ TestList [readerTests
                               ,evaluatorTests
                               ,executorTests]
     if errors v + failures v > 0
       then fail "Some tests failed."
       else return ()