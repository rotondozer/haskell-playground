module Lib
    ( getAmountIndex
    , toRowsAndColumns
    , total
    , CashFlow(..)
    )
where

import           Data.List
import           Data.List.Split
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
getAmountFromRow amountIndex row = case amount of
    Nothing -> 0
    Just a  -> a
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

