module Lib
    ( rollMain
    ) where

rollMain :: IO ()
rollMain = do
  putStrLn $ showTerm (RollAtom "snoc")
  putStrLn $ showTerm (RollPair
                       (RollAtom "+")
                       (RollPair
                        (RollAtom "1")
                        (RollPair
                         (RollAtom "2")
                         RollNil)))
  putStrLn $ showTerm (RollPair (RollAtom "car") (RollAtom "cdr"))
  putStrLn $ showTerm (RollPair (RollAtom "1")
                       (RollPair (RollAtom "2") (RollAtom "3")))


data RollTerm = RollNil
              | RollAtom String
              | RollPair RollTerm RollTerm

isList :: RollTerm -> Bool
isList (RollPair h RollNil) = True
isList (RollPair h t)       = isList t
isList _                    = False

printTerm :: RollTerm -> IO ()
printTerm = putStrLn . showTerm

showTerm :: RollTerm -> String
showTerm RollNil          = "()"
showTerm (RollAtom s)     = s
showTerm p@(RollPair _ _) = accumPairs p

accumPairs :: RollTerm -> String
accumPairs t = parenthesize $ accumPairs' t
  where accumPairs' (RollPair h RollNil) = showTerm h
        accumPairs' (RollPair h t)       = case t of
          (RollPair _ _) -> showTerm h ++ " " ++ (accumPairs' t)
          _              -> showTerm h ++ " . " ++ (showTerm t)

        accumPairs' _                    = undefined

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"
