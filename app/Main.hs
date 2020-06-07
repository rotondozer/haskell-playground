module Main where

import           Lib                            ( monthlyExpenseReport )
import           System.IO

main = do
    contents <- readFile "../../Downloads/EXPORT.CSV"
    let expenseReport = monthlyExpenseReport contents
    writeFile "../../Downloads/expense_report.txt" expenseReport
    print $ expenseReport
