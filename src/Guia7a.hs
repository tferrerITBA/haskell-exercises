module Guia7a
    (
        Ingrediente(..),
        Pizza(..),
        cantidadDeCapas,
        cantidadDeAceitunas,
        duplicarAceitunas,
        sinLactosa,
        aptaTolerantesLactosa,
        agregaAceitunasCorrectamente,
        conDescripcionMejorada,
        conDescripcionMejoradaWrapper,
        tiene,
        p1, p2, p3, p4, p5, p_invalida
    ) where

data ExpA = Cte Int | Sum ExpA ExpA | Mult ExpA ExpA
evalEA :: ExpA -> Int
evalEA (Cte n) = n
evalEA (Sum e1 e2) = evalEA e1 + evalEA e2
evalEA (Mult e1 e2) = evalEA e1 * evalEA e2

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving (Show, Eq)
data Pizza = Prepizza | Capa Ingrediente Pizza deriving (Show)

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa (Aceitunas i) p) = i + cantidadDeAceitunas p
cantidadDeAceitunas (Capa _ p) = cantidadDeAceitunas p

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa (Aceitunas i) p) = Capa (Aceitunas (2 * i)) (duplicarAceitunas p)
duplicarAceitunas (Capa i p) = Capa i (duplicarAceitunas p)

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa Queso p) = sinLactosa p
sinLactosa (Capa i p) = Capa i (sinLactosa p)

aptaTolerantesLactosa :: Pizza -> Bool
aptaTolerantesLactosa Prepizza = True
aptaTolerantesLactosa (Capa Queso p) = False
aptaTolerantesLactosa (Capa i p) = aptaTolerantesLactosa p

agregaAceitunasCorrectamente :: Pizza -> Bool
agregaAceitunasCorrectamente Prepizza = True
agregaAceitunasCorrectamente (Capa (Aceitunas i) p)
    | i > 0 = agregaAceitunasCorrectamente p
    | otherwise = False
agregaAceitunasCorrectamente (Capa i p) = agregaAceitunasCorrectamente p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa (Aceitunas i) (Capa (Aceitunas j) p)) = if i > 0 || j > 0
    then Capa (Aceitunas (i + j)) (conDescripcionMejorada p)
    else error "Agregaste mal las aceitunas"
conDescripcionMejorada (Capa i p) = Capa i (conDescripcionMejorada p)

conDescripcionMejoradaWrapper :: Pizza -> Pizza
conDescripcionMejoradaWrapper p = if agregaAceitunasCorrectamente p then conDescripcionMejorada2 p else error "Agregaste mal las aceitunas"

conDescripcionMejorada2 :: Pizza -> Pizza
conDescripcionMejorada2 Prepizza = Prepizza
conDescripcionMejorada2 (Capa (Aceitunas i) (Capa (Aceitunas j) p)) = Capa (Aceitunas (i + j)) (conDescripcionMejorada2 p)
conDescripcionMejorada2 (Capa i p) = Capa i (conDescripcionMejorada2 p)

tiene :: Ingrediente -> Pizza -> Bool
tiene i Prepizza = False
tiene i (Capa j p) = (i == j) || tiene i p

p1 = Prepizza
p2 = Capa Salsa p1
p3 = Capa Queso p2
p4 = Capa (Aceitunas 4) p3
p5 = Capa (Aceitunas 4) p4
p_invalida = Capa (Aceitunas (-5)) Prepizza
