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
import qualified Data.Map.Strict as Map
import Control.Monad (join)

type Result = Either String Syntax.Term

arities :: Map.Map String Int
arities = Map.fromList
  [ ("quote", 1)
  , ("atom",  1)
  , ("eq",    2)
  , ("car",   1)
  , ("cdr",   1)
  , ("cons",  2)
  , ("cond", -1) -- negative arity represents variadic
  ]

withError :: e -> Maybe a -> Either e a
withError e = maybe (Left e) Right

type SymbolTable = Map.Map String Syntax.Term

evalTerm :: SymbolTable -> Syntax.Term -> Result
evalTerm table (Syntax.Pair (Syntax.Atom s) rest) =
  case Map.lookup s table of
    Just body -> evalTerm table body
    Nothing   ->
      case Map.lookup s arities of
        Just arity -> evalBuiltin table s arity rest
        Nothing    -> Left $ "evalTerm: No such symbol '" ++ s ++ "'"
evalTerm _ term = pure term

evalBuiltin :: SymbolTable -> String -> Int -> Syntax.Term -> Result
evalBuiltin table s expectedArity t = do
  {- expectedArity <- withError
    ("evalBuiltin: No such builtin '" ++ s ++ "'")
    (arities Map.! s) -}
  calledArity <- withError
    ("evalBuiltin: Improper argument list " ++
      (Syntax.showTerm t)
      ++ " to builtin '" ++ s ++ "'")
    (Syntax.termLength t)
  argumentList <- withError
    ("evalBuiltin: Internal error: Failed to convert argument list " ++
      (Syntax.showTerm t) ++
      "while evaluating call to builtin '" ++ s ++ "'")
    (Syntax.termToTermList t)
  if expectedArity > 0 && calledArity /= expectedArity
    then Left $
      "evalBuiltin: Incorrect arity " ++ (show calledArity) ++
      " in call to builtin '" ++ s ++ "'. (Expected " ++
      (show expectedArity) ++ ")"
    else callBuiltin table s argumentList

callBuiltin :: SymbolTable -> String -> [Syntax.Term] -> Result
callBuiltin table name args =
  join $ withError
    ("callBuiltin: Internal error: Unknown builtin '" ++ name ++ "'")
    (lookup name builtinTable)
  where builtinTable =
          let arg1    = head args
              arg2    = head argrest
              argrest = tail args
          in [ ("quote", pure $ quote arg1      )
             , ("atom",  pure $ atom  arg1      )
             , ("eq",    pure $ eq    arg1 arg2 )
             , ("car",          car   arg1      )
             , ("cdr",          cdr   arg2      )
             , ("cons",         cons  arg1 arg2 )
             , ("cond",         cond  table args)
             ]

cond :: SymbolTable -> [Syntax.Term] -> Result
cond _ [] = pure Syntax.Nil
cond table ((Syntax.Pair condition (Syntax.Pair cmd Syntax.Nil)):rest) = do
    evalThis <- evalTerm table condition
    if evalThis == Syntax.trueTerm
      then evalTerm table cmd
      else cond table rest
cond _ arg =
  Left $ "cond: malformed argument " ++ (Syntax.showTerm (head arg)) ++
    ". The arguments of cond must be 2-element lists."

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
