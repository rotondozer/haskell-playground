module Main where

import           Lib
import           System.IO
import           Data.List
import           Data.List.Split
import           Text.Read

main = do
    contents <- readFile "../../Downloads/EXPORT.CSV"
    let rowsAndColumns = toRowsAndColumns contents
    case (getAmountIndex rowsAndColumns) of
        Nothing -> putStrLn "whoops!"
        Just index ->
            let (debits, credits) =
                        ( (totalDebits index rowsAndColumns)
                        , (totalCredits index rowsAndColumns)
                        )
            in  print
                    $  "DEBITS: "
                    ++ (show debits)
                    ++ " / CREDITS: "
                    ++ (show credits)
                    ++ " / NET: "
                    ++ (show (credits + debits))


toRowsAndColumns :: String -> [[String]]
toRowsAndColumns contents = map (splitOneOf ",\\") tableRows
    where tableRows = lines contents

getAmountIndex :: [[String]] -> Maybe Int
getAmountIndex rowsAndColumns = elemIndex "\"Amount\"" (head rowsAndColumns)

getAmountFromRow :: Int -> [String] -> Float
getAmountFromRow amountIndex row = case amount of
    Nothing -> 0
    Just a  -> a
    where amount = readMaybe (row !! amountIndex) :: Maybe Float

totalDebits :: Int -> [[String]] -> Float
totalDebits amountIndex = foldl
    (\acc row ->
        if (amountFromRow row) < 0 then acc + (amountFromRow row) else acc
    )
    0
    where amountFromRow = getAmountFromRow amountIndex

totalCredits :: Int -> [[String]] -> Float
totalCredits amountIndex = foldl
    (\acc row ->
        if (amountFromRow row) > 0 then acc + (amountFromRow row) else acc
    )
    0
    where amountFromRow = getAmountFromRow amountIndex
