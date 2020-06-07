module Lib
    ( monthlyExpenseReport
    )
where

import           Data.List
import           Data.List.Split
import           Data.Maybe                     ( fromMaybe )
import           Text.Read

type Account = String
type Row = [Account]
type RowsAndColumns = [Row] -- TODO: what's the real way of modeling 2d matrix?

data CashFlow = Debit Account Float | Credit  Account Float  -- TODO: Debit NegativeFloat | Credit ZeroOrGreaterFloat
type CashFlowReport = [CashFlow]

-- CSV -> RowsAndColumns
-- [ ["Transaction Type", "Date",   "Account Type", "Description", "Amount", "Reference No.", "Credits", "Debits"]
-- , ["DEBIT",            "6/2/20", "Checking",     "VENMO",       "-100",   "",              "",        "-$100"]
-- , ["CREDIT",           "6/3/20", "Checking",     "PAYCHECK",    "500",    "",              "$500",    ""]
-- ]
toRowsAndColumns :: String -> RowsAndColumns
toRowsAndColumns contents = map (splitOneOf ",\\") tableRows
    where tableRows = lines contents

getAmountIndex :: RowsAndColumns -> Maybe Int
getAmountIndex rowsAndColumns = elemIndex "\"Amount\"" (head rowsAndColumns)


columnIndex :: String -> RowsAndColumns -> Maybe Int
columnIndex column rowsAndColumns = elemIndex column (head rowsAndColumns)

getAmountFromRow :: Int -> Row -> Float
getAmountFromRow amountIndex row = fromMaybe 0 amount
    where amount = readMaybe (row !! amountIndex) :: Maybe Float

total :: Int -> RowsAndColumns -> (Float, Float)
total columnIndex = foldl
    (\(debits, credits) row -> case (toCashFlow (amountFromRow row)) of
        Debit  account d -> (debits + d, credits)
        Credit account c -> (debits, credits + c)
    )
    (0, 0)
    where amountFromRow = getAmountFromRow columnIndex

toCashFlow :: Float -> CashFlow
toCashFlow amount = if amount < 0.0 then Debit "" amount else Credit "" amount

monthlyExpenseReport :: String -> String
monthlyExpenseReport csvFileContents =
    case (columnIndex "\"Amount\"" rowsAndColumns) of
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
