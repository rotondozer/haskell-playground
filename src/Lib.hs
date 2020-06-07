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

getCashFlowAmt :: ColumnIndex -> Row -> Float
getCashFlowAmt amountColumn row = fromMaybe 0 amount
    where amount = readMaybe (row !! amountColumn) :: Maybe Float

getAcctDescription :: ColumnIndex -> Row -> String
getAcctDescription descColumn row = fromMaybe "NO DESCRIPTION" description
    where description = readMaybe (row !! descColumn) :: Maybe String

cashFlowReport :: ColumnIndex -> ColumnIndex -> RowsAndColumns -> CashFlowReport
cashFlowReport amtColIndex descColIndex = map
    (\row -> (toCashFlow row getAmount getDescription))
  where
    getAmount      = getCashFlowAmt amtColIndex
    getDescription = getAcctDescription descColIndex

total :: CashFlowReport -> (Float, Float)
total = foldl
    (\(debits, credits) cashFlow -> case cashFlow of
        Debit  _ d -> (debits + d, credits)
        Credit _ c -> (debits, credits + c)
    )
    (0, 0)

toCashFlow :: Row -> (Row -> Float) -> (Row -> String) -> CashFlow
toCashFlow row getAmount getDescription = if amount < 0
    then Debit (getDescription row) amount
    else Credit (getDescription row) amount
    where amount = getAmount row

monthlyExpenseReport :: String -> String
monthlyExpenseReport csvFileContents = case cashFlowColumn rowsAndColumns of
    Nothing -> "ERROR: Could not find \"Amount\" column in CSV"
    Just cfColumn ->
        let (debits, credits) =
                    total (cashFlowReport cfColumn descColumn rowsAndColumns)
        in  "DEBITS: "
                ++ (show debits)
                ++ " / CREDITS: "
                ++ (show credits)
                ++ " / NET: "
                ++ (show $ credits + debits)
  where
    rowsAndColumns = toRowsAndColumns csvFileContents
    descColumn     = fromMaybe 3 (acctDescriptionColumn rowsAndColumns) -- TODO
