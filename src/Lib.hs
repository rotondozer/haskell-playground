module Lib
    ( someFunc
    )
where

someFunc :: Int
someFunc = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))
