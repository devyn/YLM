import Test.HUnit hiding (Label)

import YLM.Elem
import YLM.TextInterface
import YLM.Runtime
import YLM.Interfaces.Standard
import YLM.Runtimes.Standard
import Data.Map (Map)
import qualified Data.Map as Map

elemReadEmptyStringZero =
  TestCase $ assertEqual "Reading an empty string returns an empty list"
               (Right [])
               (ylmRead (Standard Map.empty) "")

elemReadSingleLabel =
  TestCase $ assertEqual "Reading a single label returns a single Label"
               (Right [Label "hello"])
               (ylmRead (Standard Map.empty) "hello")

elemReadMultiLists =
  TestCase $ assertEqual "Reading () (a) (a b) returns appropriate values"
               (Right [Nil
                      , Cons (Label "a") Nil
                      , Cons (Label "a") (Cons (Label "b") Nil)])
               (ylmRead (Standard Map.empty) "() (a) (a b)")

elemReadAssocList =
  TestCase $ assertEqual "Reader can handle association lists"
               (Right [Cons (Cons (Label "a") (Label "b"))
                            (Cons (Cons (Label "c") (Label "d")) Nil)])
               (ylmRead (Standard Map.empty) "((a . b) (c . d))")

elemReadQuote =
  TestCase $ assertEqual "Reader properly expands the (quote) form for a quoted input"
               (Right [Cons (Label "quote")
                            (Cons (Label "a") Nil)
                      ,Cons (Label "quote")
                            (Cons (Cons (Label "a") (Cons (Label "b") Nil)) Nil)])
               (ylmRead (Standard Map.empty) "'a '(a b)")

elemReadQuotedQuote =
  TestCase $ assertEqual "Reader properly expands ''a as (quote (quote a))"
               (Right [Cons (Label "quote")
                            (Cons (Cons (Label "quote") (Cons (Label "a") Nil)) Nil)])
               (ylmRead (Standard Map.empty) "''a")

elemReadMathExpr =
  TestCase $ assertEqual "Reader properly parses mathematical expressions including numbers"
               (Right [Cons (Label "+")
                            (Cons (NumInt 1)
                                  (Cons (NumFloat 2.0)
                                        (Cons (Cons (Label "-")
                                                    (Cons (NumInt 6)
                                                          (Cons (NumInt 1) Nil))) Nil)))])
               (ylmRead (Standard Map.empty) "(+ 1 2.0 (- 6 1))")

elemReadEscapeCode =
  TestCase $ assertEqual "Reader properly recognizes escape codes in long form labels"
               (Right [Label "hello world \"\r\n\" \0 \a \b \f \t \v \ESC"])
               (ylmRead (Standard Map.empty)
                "\"hello world \\\"\\r\\n\\\" \\0 \\a \\b \\f \\t \\v \\e\"")

elemReadWhitespace =
  TestCase $ assertEqual "Reader properly ignores whitespace including newlines"
               (Right [Cons (Label "a") (Cons (Label "b") (Cons (Label "c") Nil))])
               (ylmRead (Standard Map.empty)
                "\t( a\n\tb\r\nc\n)\t\n")

elemReadLookupFunction =
  TestCase $ assertEqual "Reader properly reads definition for the (lookup) function"
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

main =
  runTestTT $ TestList [readerTests]