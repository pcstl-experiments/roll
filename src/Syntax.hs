module Syntax
  ( Term(..),
    printTerm,
    showTerm,
    isProperList
  ) where

data Term = Nil
          | Atom String
          | Pair Term Term

printTerm :: Term -> IO ()
printTerm = putStrLn . showTerm

showTerm :: Term -> String
showTerm Nil          = "()"
showTerm (Atom s)     = s
showTerm p@(Pair _ _) = showPair p

showPair :: Term -> String
showPair t = parenthesize $ showPair' t
  where showPair' (Pair h Nil) = showTerm h
        showPair' (Pair h t)       = case t of
          (Pair _ _) -> showTerm h ++ " "   ++ (showPair' t)
          _          -> showTerm h ++ " . " ++ (showTerm t)

        -- this final case should never happen, as showPair should only ever
        -- be called by showTerm and related functions.
        showPair' _                    =
          error "Roll.showPair: Attempt to display non-cons-cell as cons cell"

isProperList :: Term -> Bool
isProperList (Pair h Nil) = True
isProperList (Pair h t)   = isProperList t
isProperList _            = False

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"
