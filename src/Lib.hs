module Lib
    ( getAmountFromRow
    , getAmountIndex
    , toRowsAndColumns
    , totalCredits
    , totalDebits
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

totalDebits :: Int -> RowsAndColumns -> Float
totalDebits amountIndex = foldl
    (\acc row ->
        if (amountFromRow row) < 0 then acc + (amountFromRow row) else acc
    )
    0
    where amountFromRow = getAmountFromRow amountIndex

totalCredits :: Int -> RowsAndColumns -> Float
totalCredits amountIndex = foldl
    (\acc row ->
        if (amountFromRow row) > 0 then acc + (amountFromRow row) else acc
    )
    0
    where amountFromRow = getAmountFromRow amountIndex
