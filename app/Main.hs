module Main where

import           Lib                            ( monthlyExpenseReport )
import           System.IO

main = do
    contents <- readFile "../../Downloads/EXPORT.CSV"
    print $ monthlyExpenseReport contents
