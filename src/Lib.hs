module Lib
    ( monthlyExpenseReport
    )
where

import           Data.List
import           Data.List.Split
import           Data.Maybe                     ( fromMaybe )
import           Text.Read

type Row = [String]
type RowsAndColumns = [Row]

data CashFlow = Credit Float | Debit Float

toRowsAndColumns :: String -> RowsAndColumns
toRowsAndColumns contents = map (splitOneOf ",\\") tableRows
    where tableRows = lines contents

getAmountIndex :: RowsAndColumns -> Maybe Int
getAmountIndex rowsAndColumns = elemIndex "\"Amount\"" (head rowsAndColumns)

getAmountFromRow :: Int -> Row -> Float
getAmountFromRow amountIndex row = fromMaybe 0 amount
    where amount = readMaybe (row !! amountIndex) :: Maybe Float


total :: Int -> RowsAndColumns -> (Float, Float) -- TODO: type (NegativeFloat, ZeroOrGreaterFloat)
total columnIndex = foldl
    (\(debits, credits) row -> case (toCashFlow (amountFromRow row)) of
        Debit  d -> (debits + d, credits)
        Credit c -> (debits, credits + c)
    )
    (0, 0)
    where amountFromRow = getAmountFromRow columnIndex

toCashFlow :: Float -> CashFlow
toCashFlow amount | amount < 0.0 = Debit amount
                  | otherwise    = Credit amount


monthlyExpenseReport :: String -> String
monthlyExpenseReport csvFileContents = case (getAmountIndex rowsAndColumns) of
    Nothing -> "ERROR: Could not find \"Amount\" column in CSV"
    Just index ->
        let (debits, credits) = total index rowsAndColumns
        in  "DEBITS: "
                ++ (show debits)
                ++ " / CREDITS: "
                ++ (show credits)
                ++ " / NET: "
                ++ (show $ credits + debits)
    where rowsAndColumns = toRowsAndColumns csvFileContents
