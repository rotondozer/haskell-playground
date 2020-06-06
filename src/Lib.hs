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

data CashFlow = Credit | Debit

toRowsAndColumns :: String -> RowsAndColumns
toRowsAndColumns contents = map (splitOneOf ",\\") tableRows
    where tableRows = lines contents

getAmountIndex :: RowsAndColumns -> Maybe Int
getAmountIndex rowsAndColumns = elemIndex "\"Amount\"" (head rowsAndColumns)

getAmountFromRow :: Int -> Row -> Float
getAmountFromRow amountIndex row = fromMaybe 0 amount
    where amount = readMaybe (row !! amountIndex) :: Maybe Float

total :: CashFlow -> Int -> RowsAndColumns -> Float
total cashFlow columnIndex = foldl
    (\acc row ->
        let amount = amountFromRow row
        in  if (amount `isOfCashFlowType` cashFlow) then acc + amount else acc
    )
    0
    where amountFromRow = getAmountFromRow columnIndex

isOfCashFlowType :: Float -> CashFlow -> Bool
isOfCashFlowType amount cashFlow = case cashFlow of
    Debit  -> (amount < 0.0)
    Credit -> (amount > 0.0)

monthlyExpenseReport :: String -> String
monthlyExpenseReport csvFileContents =
    let rowsAndColumns = toRowsAndColumns csvFileContents
    in  case (getAmountIndex rowsAndColumns) of
            Nothing -> "whoops!"
            Just index ->
                let (debits, credits) =
                            ( (total Debit index rowsAndColumns)
                            , (total Credit index rowsAndColumns)
                            )
                in  "DEBITS: "
                        ++ (show debits)
                        ++ " / CREDITS: "
                        ++ (show credits)
                        ++ " / NET: "
                        ++ (show $ credits + debits)
