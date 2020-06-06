module Main where

import           Lib
import           System.IO
import           Data.List
import           Data.List.Split

main = do
    contents <- readFile "../../Downloads/EXPORT.CSV"
    let rowsAndColumns = toRowsAndColumns contents
    case (getAmountIndex rowsAndColumns) of
        Nothing    -> print "whoops!"
        Just index -> print $ totalDebits index rowsAndColumns
        -- Just index -> print $ getAmountFromRow index (rowsAndColumns !! 1)


toRowsAndColumns :: String -> [[String]]
toRowsAndColumns contents = map (splitOneOf ",\\") tableRows
    where tableRows = lines contents

getAmountIndex :: [[String]] -> Maybe Int
getAmountIndex rowsAndColumns =
    let stuff = elemIndex "\"Amount\"" (head rowsAndColumns) in stuff

getAmountFromRow :: Int -> [String] -> Float
getAmountFromRow amountIndex row = read (row !! amountIndex) :: Float

totalDebits :: Int -> [[String]] -> Float
totalDebits amountIndex = foldl
    (\acc row -> if (head row) == "DEBIT"
        then acc + (getAmountFromRow amountIndex row)
        else acc
    )
    0


