module Guia5
    (
        DigBin(..),
        dbAsInt
    ) where

data DigBin = O | I deriving(Show, Eq)

dbAsInt :: DigBin -> Int
dbAsInt I = 1
dbAsInt O = 0
