module Guia7b
    (
        Planilla(..),
        Equipo(..),
        largoDePlanilla,
        esta,
        juntarPlanillas,
        nivelesJerarquicos,
        cantidadDeIntegrantes,
        planillaDeIntegrantes,
        pl1, pl2, pl3, pl4, pll1, pll2, pll3, be1, be2, be3, be4, be5, e1, e2,
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

type Nombre = String
data Planilla = Fin | Registro Nombre Planilla deriving(Show)
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo deriving(Show)

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

esta :: Nombre -> Planilla -> Bool
esta n Fin = False
esta n (Registro m p) = n == m || esta n p

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p = p
juntarPlanillas (Registro n1 p1) p2 = Registro n1 (juntarPlanillas p1 p2)

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n) = 0
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + max (nivelesJerarquicos e1) (max (nivelesJerarquicos e2) (nivelesJerarquicos e3))

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n) = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n) = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanillas (planillaDeIntegrantes e1) (juntarPlanillas (planillaDeIntegrantes e2) (planillaDeIntegrantes e3)))

pl1 = Fin
pl2 = Registro "Juan" pl1
pl3 = Registro "Marcos" pl2
pl4 = Registro "Palito" pl3

pll1 = Fin
pll2 = Registro "Guido" pll1
pll3 = Registro "Tomas" pll2

be1 = Becario "Marcos"
be2 = Becario "Guido"
be3 = Becario "Tomas"
be4 = Becario "Pablo"
be5 = Becario "Pedro"

e1 = Investigador "Palito" be1 be2 be3
e2 = Investigador "Tato" be4 be5 e1
