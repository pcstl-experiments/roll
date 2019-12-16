module Lib
    ( rollMain
    ) where

import qualified Syntax 

rollMain :: IO ()
rollMain = do
  putStrLn $ Syntax.showTerm (Syntax.Atom "snoc")
  putStrLn $ Syntax.showTerm (Syntax.Pair
                              (Syntax.Atom "+")
                              (Syntax.Pair
                               (Syntax.Atom "1")
                               (Syntax.Pair
                                (Syntax.Atom "2")
                                Syntax.Nil)))
  putStrLn $ Syntax.showTerm (Syntax.Pair
                              (Syntax.Atom "car")
                              (Syntax.Atom "cdr"))
  putStrLn $ Syntax.showTerm (Syntax.Pair (Syntax.Atom "1")
                       (Syntax.Pair (Syntax.Atom "2") (Syntax.Atom "3")))
