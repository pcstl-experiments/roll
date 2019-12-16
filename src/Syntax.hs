module Syntax
  ( Term(..),
    printTerm,
    showTerm,
    isAtom,
    isProperList,
    boolToTerm,
    listToTerm,
    termToTermList,
    termToStringList,
    trueTerm,
    falseTerm,
    termLength
  ) where

data Term = Nil
          | Atom String
          | Pair Term Term
          deriving (Eq, Show)

trueTerm  = Atom "t"
falseTerm = Nil

printTerm :: Term -> IO ()
printTerm = putStrLn . showTerm

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

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

isAtom :: Term -> Bool
isAtom (Atom _) = True
isAtom _        = False

isProperList :: Term -> Bool
isProperList (Pair h Nil) = True
isProperList (Pair h t)   = isProperList t
isProperList _            = False

boolToTerm :: Bool -> Term
boolToTerm True  = trueTerm
boolToTerm False = falseTerm

termLength :: Term -> Maybe Int
termLength t = termLength' t 0
  where termLength' (Pair _ rest) acc = termLength' rest (acc+1)
        termLength' Nil           acc = Just acc
        termLength' _             _   = Nothing

-- functions from here on are mostly used for testing and should probably be
-- less trusted than the rest here. E.g.: They mostly only work on proper lists.
listToTerm :: [String] -> Term
listToTerm = foldr Pair Nil . (map Atom)

termToTermList :: Term -> Maybe [Term]
termToTermList l = reverse <$> termToList' l []
  where termToList' Nil        acc = Just acc
        termToList' (Pair h t) acc = termToList' t (h:acc)
        termToList' _          _   = Nothing

termToStringList :: Term -> Maybe [String]
termToStringList t = termToTermList t >>= (sequence . (map toStringIfAtom))
  where toStringIfAtom (Atom s) = Just s
        toStringIfAtom _        = Nothing
