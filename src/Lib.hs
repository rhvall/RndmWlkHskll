-- #                    GNU GENERAL PUBLIC LICENSE
-- #                       Version 3, 29 June 2007
-- #
-- # Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
-- # Everyone is permitted to copy and distribute verbatim copies
-- # of this license document, but changing it is not allowed.

module Lib
-- (
--     readPair,
--     readSeed
-- )
where

import           Data.List.Split
import           Graphics.EasyPlot
import           Text.CSV

data RndWlkDt = RndWlkDt Int [Float]
    deriving (Read, Show)

readElems :: [Field] -> [Float]
readElems []       = []
readElems xs = map read elemsPart
    where elemsPart = tail $ dropWhile (/= "elems") xs

readSeed :: [Field] -> Int
readSeed []           = -1
readSeed (x : y : xs) = read y

readRndWlk :: [Field] -> RndWlkDt
readRndWlk fields = RndWlkDt (readSeed fields) (readElems fields)

plotRndWalk :: IO [Bool]
plotRndWalk = do
    parsed <- parseCSVFromFile "RandomWalk.csv"
    let noEmpty = either (const []) (filter (\x -> 2 <= length x))
    let base = noEmpty parsed
    let rnds = map readRndWlk base
    let plotPNG path = plot (PNG path)
    let pairedRnd (RndWlkDt _ fltLst) = zip [0..] fltLst
    let rndsPair = foldr (\x y -> y ++ [(fst (last y) + 1, pairedRnd x)]) [(1, pairedRnd $ head rnds)] (tail rnds)
    let baseName = "resources/Plot"
    let baseExt = ".png"
    let replaceBase num = baseName ++ show num ++ baseExt
    let metaPlot = Data2D [Style Lines, Title "Random Walk", Color Blue]
    let plotWithNum num paired = plotPNG (replaceBase num) $ metaPlot [] paired
    traverse (uncurry plotWithNum) rndsPair
