import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import qualified Syntax

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [unitTests, propertyTests]

propertyTests :: TestTree
propertyTests = testGroup "Syntax property tests"
  [ QC.testProperty "Showing atom yields atom name" $
      \name ->
        (Syntax.showTerm (Syntax.Atom name)) == name
  , QC.testProperty "Showing improper list yields dotted pair" $
      \x y ->
        (Syntax.showTerm (Syntax.Pair (Syntax.Atom x) (Syntax.Atom y)))
          == "(" ++ x ++ " . " ++ y ++ ")"
  , QC.testProperty "Showing single element list yields parenthesized term" $
      \x ->
        (Syntax.showTerm (Syntax.Pair (Syntax.Atom x) Syntax.Nil))
          == "(" ++ x ++ ")"
  , QC.testProperty "Showing proper list yields list representation" $
      \x y ->
        (Syntax.showTerm (Syntax.Pair
                          (Syntax.Atom x)
                          (Syntax.Pair
                           (Syntax.Atom y)
                           Syntax.Nil)))
          == "(" ++ x ++ " " ++ y ++ ")"
  ]

unitTests :: TestTree
unitTests = testGroup "Syntax unit tests"
  [ testCase "Showing nil yields empty list" $
      Syntax.showTerm Syntax.Nil @?= "()"
  , testCase "Showing complex improper list yields correct representation" $
      (Syntax.showTerm (Syntax.Pair
                        (Syntax.Atom "1")
                        (Syntax.Pair
                         (Syntax.Atom "2")
                         (Syntax.Atom "3")))) @?= "(1 2 . 3)"
  ]


