module Lib
    ( monthlyExpenseReport
    )
where

import           Data.List
import           Data.List.Split
import           Data.Maybe                     ( fromMaybe )
import           Text.Read

type ColumnIndex = Int
type Row = [String]

type RowsAndColumns = [Row] -- TODO: what's the real way of modeling 2d matrix?

type Account = String
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

cashFlowColumn :: RowsAndColumns -> Maybe ColumnIndex
cashFlowColumn rowsAndColumns = elemIndex "\"Amount\"" (head rowsAndColumns)

acctDescriptionColumn :: RowsAndColumns -> Maybe ColumnIndex
acctDescriptionColumn rowsAndColumns =
    elemIndex "\"Description\"" (head rowsAndColumns)

getAmountFromRow :: ColumnIndex -> Row -> Float
getAmountFromRow amountColumn row = fromMaybe 0 amount
    where amount = readMaybe (row !! amountColumn) :: Maybe Float

total :: ColumnIndex -> RowsAndColumns -> (Float, Float)
total amtColIndex = foldl
    (\(debits, credits) row -> case (toCashFlow . amountFromRow) row of
        Debit  account d -> (debits + d, credits)
        Credit account c -> (debits, credits + c)
    )
    (0, 0)
    where amountFromRow = getAmountFromRow amtColIndex

toCashFlow :: Float -> CashFlow
toCashFlow amount = if amount < 0.0 then Debit "" amount else Credit "" amount

monthlyExpenseReport :: String -> String
monthlyExpenseReport csvFileContents = case cashFlowColumn rowsAndColumns of
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
