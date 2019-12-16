import Test.Tasty                  (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck as QC ((==>), testProperty)
import Test.Tasty.HUnit            (Assertion, testCase, assertBool,
                                    assertFailure, (@?=))

import qualified Syntax
import qualified Builtins

main :: IO ()
main = defaultMain tests

assertSuccess :: Assertion
assertSuccess = assertBool "" True

tests :: TestTree
tests = testGroup "All tests" [syntaxTests, builtinTests]

syntaxTests :: TestTree
syntaxTests = testGroup "Syntax tests" [syntaxUnitTests, syntaxPropertyTests]

syntaxPropertyTests :: TestTree
syntaxPropertyTests = testGroup "Syntax property tests"
  [ testProperty "Showing atom yields atom name" $
      \name ->
        (Syntax.showTerm (Syntax.Atom name)) == name
  , testProperty "Showing improper list yields dotted pair" $
      \x y ->
        (Syntax.showTerm (Syntax.Pair (Syntax.Atom x) (Syntax.Atom y)))
          == "(" ++ x ++ " . " ++ y ++ ")"
  , testProperty "Showing single element list yields parenthesized term" $
      \x ->
        (Syntax.showTerm (Syntax.Pair (Syntax.Atom x) Syntax.Nil))
          == "(" ++ x ++ ")"
  , testProperty "Showing proper list yields list representation" $
      \x y ->
        (Syntax.showTerm (Syntax.Pair
                          (Syntax.Atom x)
                          (Syntax.Pair
                           (Syntax.Atom y)
                           Syntax.Nil)))
          == "(" ++ x ++ " " ++ y ++ ")"
  , testProperty "Converting to term and back to list works" $
      \l ->
        case Syntax.termToStringList (Syntax.listToTerm l) of
          Just list -> list == l
          Nothing   -> False
  ]

syntaxUnitTests :: TestTree
syntaxUnitTests = testGroup "Syntax unit tests"
  [ testCase "Showing nil yields empty list" $
      Syntax.showTerm Syntax.Nil @?= "()"
  , testCase "Showing complex improper list yields correct representation" $
      (Syntax.showTerm (Syntax.Pair
                        (Syntax.Atom "1")
                        (Syntax.Pair
                         (Syntax.Atom "2")
                         (Syntax.Atom "3")))) @?= "(1 2 . 3)"
  , testCase "Showing complex proper list yields correct representation" $
      (Syntax.showTerm (Syntax.Pair
                        (Syntax.Atom "a")
                        (Syntax.Pair
                         (Syntax.Atom "b")
                         (Syntax.Pair
                          (Syntax.Atom "c")
                          Syntax.Nil)))) @?= "(a b c)"
  , testCase "Converting list to term works" $
      Syntax.listToTerm ["a", "b", "c"] @?= (Syntax.Pair
                                             (Syntax.Atom "a")
                                             (Syntax.Pair
                                              (Syntax.Atom "b")
                                              (Syntax.Pair
                                               (Syntax.Atom "c")
                                               Syntax.Nil)))
  , testCase "Converting true to term works" $
      Syntax.boolToTerm True @?= Syntax.trueTerm
  , testCase "Converting false to term works" $
      Syntax.boolToTerm False @?= Syntax.falseTerm
  ]

builtinTests :: TestTree
builtinTests = testGroup "Built-in tests"
  [ builtinUnitTests
  , builtinPropertyTests
  ]

builtinUnitTests :: TestTree
builtinUnitTests = testGroup "Built-in unit tests"
  [ testCase "quote nil is nil" $
      Builtins.quote Syntax.Nil @?= Syntax.Nil
  , testCase "nil is not an atom" $
      Builtins.atom Syntax.Nil @?= Syntax.falseTerm
  , testCase "nil is equal to itself" $
      Builtins.eq Syntax.Nil Syntax.Nil @?= Syntax.trueTerm
  , testCase "cannot take car of nil" $
      case Builtins.car Syntax.Nil of
        (Right _) ->
          assertFailure "Somehow took car of nil!"
        (Left _) ->
          assertSuccess
  , testCase "cannot take cdr of nil" $
      case Builtins.cdr Syntax.Nil of
        (Right _) ->
          assertFailure "Somehow took cdr of nil!"
        (Left _) ->
          assertSuccess
  , testCase "cons nil onto nil yields pair" $
      case Builtins.cons Syntax.Nil Syntax.Nil of
        (Right (Syntax.Pair Syntax.Nil Syntax.Nil)) ->
          assertSuccess
        (Left _) ->
          assertFailure "Failed to cons nil onto nil"
  , testCase "pair of nil and nil is proper list" $
      case Builtins.cons Syntax.Nil Syntax.Nil of
        (Right pair) ->
          assertBool "(()) is not a list" $
            Syntax.isProperList pair
        (Left _) ->
          assertFailure "Failed to cons nil onto nil"
  ]

builtinPropertyTests :: TestTree
builtinPropertyTests = testGroup "Built-in property tests"
  [ testProperty "Quote is atom identity function" $
      \s ->
        let atom = Syntax.Atom s in Builtins.quote atom == atom
  , testProperty "Quote is pair identity function" $
      \x y ->
        let ax    = Syntax.Atom x
            ay    = Syntax.Atom y
            pair  = Syntax.Pair ax ay
        in Builtins.quote pair == pair
  , testProperty "Quote is list identity function (limited size)" $
      \x y ->
        let ax    = Syntax.Atom x
            ay    = Syntax.Atom y
            list  = Syntax.Pair ax (Syntax.Pair ay Syntax.Nil)
        in Builtins.quote list == list
  , testProperty "Quote is list identity function (random)" $
      \l ->
        let list = Syntax.listToTerm l in Builtins.quote list == list
  , testProperty "Every atom is an atom" $
      \s ->
        let as = Syntax.Atom s in Builtins.atom as == Syntax.trueTerm
  , testProperty "A pair of atoms is not an atom" $
      \x y ->
        let ax   = Syntax.Atom x
            ay   = Syntax.Atom y
            pair = Syntax.Pair ax ay
        in Builtins.atom pair == Syntax.falseTerm
  , testProperty "Every list is not an atom" $
      \l ->
        let list = Syntax.listToTerm l in Builtins.atom list == Syntax.falseTerm
  , testProperty "Every atom is equal to itself" $
      \x ->
        Builtins.eq (Syntax.Atom x) (Syntax.Atom x) == Syntax.trueTerm
  , testProperty "Every atom is different from a different atom" $
      \x y ->
        x /= y ==>
          Builtins.eq (Syntax.Atom x) (Syntax.Atom y) == Syntax.falseTerm
  , testProperty "Every pair is equal to itself" $
      \x y ->
        let ax   = Syntax.Atom x
            ay   = Syntax.Atom y
            pair = Syntax.Pair ax ay
        in Builtins.eq pair pair == Syntax.trueTerm
  , testProperty "Every pair is different from a different pair" $
      \x y z w ->
        x /= z || y /= w ==>
          let ax    = Syntax.Atom x
              ay    = Syntax.Atom y
              az    = Syntax.Atom z
              aw    = Syntax.Atom w
              pair1 = Syntax.Pair ax ay
              pair2 = Syntax.Pair az aw
          in Builtins.eq pair1 pair2 == Syntax.falseTerm
  , testProperty "Every list is equal to itself" $
      \l ->
        let list = Syntax.listToTerm l
        in Builtins.eq list list == Syntax.trueTerm
  , testProperty "Every list is different from a different list" $
      \l1 l2 ->
        l1 /= l2 ==>
          let list1 = Syntax.listToTerm l1
              list2 = Syntax.listToTerm l2
          in Builtins.eq list1 list2 == Syntax.falseTerm
  , testProperty "No atom has a car" $
      \s ->
        not $ Builtins.resultToBool (Builtins.car $ Syntax.Atom s)
  , testProperty "No atom has a cdr" $
      \s ->
        not $ Builtins.resultToBool (Builtins.cdr $ Syntax.Atom s)
  , testProperty "Every pair has a car" $
      \x y ->
        let ax   = Syntax.Atom x
            pair = Syntax.Pair ax (Syntax.Atom y)
        in case Builtins.car pair of
          (Right term) -> term == ax
          _            -> False
  , testProperty "Every pair has a cdr" $
      \x y ->
        let ay   = Syntax.Atom y
            pair = Syntax.Pair (Syntax.Atom x) ay
        in case Builtins.cdr pair of
          (Right term) -> term == ay
          _            -> False
  , testProperty "Every non-empty list has a car" $
      \l ->
        length l > 0 ==>
          let listHead = Syntax.Atom (head l)
              list     = Syntax.listToTerm l
          in case Builtins.car list of
            (Right term) -> term == listHead
            _            -> False
  , testProperty "Every non-empty list has a cdr" $
      \l ->
        length l > 0 ==>
          let listTail = Syntax.listToTerm (tail l)
              list     = Syntax.listToTerm l
          in case Builtins.cdr list of
            (Right term) -> term == listTail
            _            -> False
  , testProperty "Cons nil increases length by 1" $
      \l ->
        let consed = Builtins.cons Syntax.Nil (Syntax.listToTerm l)
        in case Syntax.termToTermList <$> consed of
          (Right (Just list)) -> length list == length l + 1
          _                   -> False
  , testProperty "Cons atom increases length by 1" $
      \l s ->
        let consed = Builtins.cons (Syntax.Atom s) (Syntax.listToTerm l)
        in case Syntax.termToTermList <$> consed of
          (Right (Just list)) -> length list == length l + 1
          _                   -> False
  , testProperty "Cons list increases length by 1 (right)" $
      \l1 l2 ->
        let consed = Builtins.cons
              (Syntax.listToTerm l1)
              (Syntax.listToTerm l2)
        in case Syntax.termToTermList <$> consed of
          (Right (Just list)) -> length list == length l2 + 1
          _                   -> False
  , testProperty "Cons list increases length by 1 (left)" $
      \l1 l2 ->
        let consed = Builtins.cons
              (Syntax.listToTerm l2)
              (Syntax.listToTerm l1)
        in case Syntax.termToTermList <$> consed of
          (Right (Just list)) -> length list == length l1 + 1
          _                   -> False
  , testProperty "(car (cons x l)) is x (nil)" $
      \l ->
        let consed = Builtins.cons Syntax.Nil (Syntax.listToTerm l)
        in case Builtins.car <$> consed of
          (Right (Right Syntax.Nil)) -> True
          _                          -> False
  , testProperty "(car (cons x l)) is x (atom)" $
      \l s ->
        let consed = Builtins.cons (Syntax.Atom s) (Syntax.listToTerm l)
        in case Builtins.car <$> consed of
          (Right (Right (Syntax.Atom s'))) -> s == s'
          _                                -> False
  , testProperty "cons does not change cdr" $
      \l ->
        let termList = Syntax.listToTerm l
            consed = Builtins.cons Syntax.Nil termList
        in case Builtins.cdr <$> consed of
          (Right (Right l')) -> termList == l'
          _                 -> False
  ]
