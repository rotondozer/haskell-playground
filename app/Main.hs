module Main where

import           Lib                            ( getAmountIndex
                                                , toRowsAndColumns
                                                , total
                                                , CashFlow(..)
                                                )
import           System.IO

main = do
    contents <- readFile "../../Downloads/EXPORT.CSV"
    let rowsAndColumns = toRowsAndColumns contents
    case (getAmountIndex rowsAndColumns) of
        Nothing -> putStrLn "whoops!"
        Just index ->
            let (debits, credits) =
                        ( (total Debit index rowsAndColumns)
                        , (total Credit index rowsAndColumns)
                        )
            in  print
                    $  "DEBITS: "
                    ++ (show debits)
                    ++ " / CREDITS: "
                    ++ (show credits)
                    ++ " / NET: "
                    ++ (show (credits + debits))
