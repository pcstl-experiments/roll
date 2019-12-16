module Builtins
  ( Result,
    quote,
    atom,
    eq,
    car,
    cdr,
    cons,
    cond,
    resultToBool
  ) where

import qualified Syntax

type Result = Either String Syntax.Term

quote :: Syntax.Term -> Syntax.Term
quote t = t

atom :: Syntax.Term -> Syntax.Term
atom = Syntax.boolToTerm . Syntax.isAtom

eq :: Syntax.Term -> Syntax.Term -> Syntax.Term
eq t1 t2 = Syntax.boolToTerm (t1 == t2)

car :: Syntax.Term -> Result
car (Syntax.Pair h _) = Right h
car t                 =
  Left $ "Cannot take car of " ++ (Syntax.showTerm t)

cdr :: Syntax.Term -> Result
cdr (Syntax.Pair _ t) = Right t
cdr t                 =
  Left $ "Cannot take cdr of " ++ (Syntax.showTerm t)

cons :: Syntax.Term -> Syntax.Term -> Result
cons _ a@(Syntax.Atom _) =
  Left $ "Cannot cons onto atom " ++ (Syntax.showTerm a)
cons h t = Right $ Syntax.Pair h t

-- for testing
resultToBool :: Result -> Bool
resultToBool (Right _) = True
resultToBool (Left _)  = False
