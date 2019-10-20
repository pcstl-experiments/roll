module Lib
    ( rollMain
    ) where

import Data.List ( intercalate )

rollMain :: IO ()
rollMain = do
  putStrLn $ showTerm (RollAtom "snoc")
  putStrLn $ showTerm (RollList [RollAtom "+", RollAtom "1", RollAtom "2"])
  putStrLn $ showTerm (RollPair (RollAtom "car") (RollAtom "cdr"))


data RollTerm = RollAtom String
              | RollList [RollTerm]
              | RollPair RollTerm RollTerm

showTerm :: RollTerm -> String
showTerm (RollAtom s)   = s
showTerm (RollList ls)  = parenthesize (intercalate " " $ map showTerm ls)
showTerm (RollPair h t) = parenthesize (showTerm h ++ " . " ++ showTerm t)

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"
