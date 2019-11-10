module Guia8b
    (
        N(..),
        evalN,
        addN,
        prodN,
        int2N,
        n0, n1, n2,
        NU,
        evalNU,
        succNU,
        addNU,
        nu2N,
        n2NU,
        nu0, nu1, nu2, nu3,
        NBin,
        evalNB,
        normalizarNB
    ) where

import Guia5
import qualified Guia8a (append)

data N = Z | S N deriving(Show)

evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z n2 = n2
addN (S n1) n2 = S (addN n1 n2)

prodN :: N -> N -> N
prodN Z _ = Z
prodN (S n1) n2 = addN n2 (prodN n1 n2)

int2N :: Int -> N -- da bottom con numeros negativos
int2N 0 = Z
int2N n = S (int2N (n-1))

type NU = [()]

evalNU :: NU -> Int
evalNU [] = 0
evalNU (x:xs) = 1 + evalNU xs

succNU :: NU -> NU
succNU x = ():x

addNU :: NU -> NU -> NU
addNU [] x2 = x2
addNU (x:xs) x2 = x : addNU xs x2

nu2N :: NU -> N
nu2N [] = Z
nu2N (x:xs) = S (nu2N xs)

n2NU :: N -> NU
n2NU Z = []
n2NU (S n) = ():n2NU n

n0 = Z
n1 = S n0
n2 = S n1

nu0 = []
nu1 = [()]
nu2 = ():nu1
nu3 = ():nu2

type NBin = [DigBin]

evalNB :: NBin -> Int
evalNB [] = error "Numero vacio"
evalNB x = evalNBaux 0 x

evalNBaux :: Int -> NBin -> Int
evalNBaux n [x] = bin2dec n x
evalNBaux n (x:xs) = bin2dec n x + evalNBaux (n+1) xs

bin2dec :: Int -> DigBin -> Int
bin2dec i dig = dbAsInt dig * (2 ^ i)

normalizarNB :: NBin -> NBin
normalizarNB [] = error "Numero vacio"
normalizarNB n = let x = normalizarNBaux n [] in if x == [] then [O] else x

normalizarNBaux :: NBin -> NBin -> NBin
normalizarNBaux [] _ = []
normalizarNBaux (x:xs) aux = if x == I then Guia8a.append (Guia8a.append aux [I]) (normalizarNBaux xs []) else normalizarNBaux xs (Guia8a.append aux [O])
