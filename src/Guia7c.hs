module Guia7c
    (
        Dungeon(..),
        cantidadDeBifurcaciones,
        cantidadDePuntosInteresantes,
        cantidadDePuntosVacios,
        cantidadDePuntosCon,
        esLineal,
        llenoDe,
        h1, h2, h3, pj1, b1, b2
    ) where

data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a) deriving (Show)

cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion a) = 0
cantidadDeBifurcaciones (Pasaje _ d) = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion _ d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2

cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion a) = 1
cantidadDePuntosInteresantes (Pasaje _ d) = 1 + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion _ d1 d2) = 1 + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2

cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion a) = 0
cantidadDePuntosVacios (Pasaje Nothing d) = 1 + cantidadDePuntosVacios d
cantidadDePuntosVacios (Pasaje _ d) = cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion Nothing d1 d2) = 1 + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2
cantidadDePuntosVacios (Bifurcacion _ d1 d2) = cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

optionalCompare :: Eq a => a -> Maybe a -> Int
optionalCompare a1 (Nothing) = 0
optionalCompare a1 (Just a2) = if a1 == a2 then 1 else 0

cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon a1 (Habitacion a2) = optionalCompare a1 (Just a2)
cantidadDePuntosCon a1 (Pasaje a2 d) = optionalCompare a1 a2 + cantidadDePuntosCon a1 d
cantidadDePuntosCon a1 (Bifurcacion a2 d1 d2) = optionalCompare a1 a2 + cantidadDePuntosCon a1 d1 + cantidadDePuntosCon a1 d2

esLineal :: Dungeon a -> Bool
esLineal (Habitacion a) = True
esLineal (Pasaje _ d) = esLineal d
esLineal (Bifurcacion _ _ _) = False

llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe a1 (Habitacion a2) = a1 == a2
llenoDe a1 (Pasaje Nothing d) = llenoDe a1 d
llenoDe a1 (Pasaje (Just a2) d) = a1 == a2 && llenoDe a1 d
llenoDe a1 (Bifurcacion Nothing d1 d2) = llenoDe a1 d1 && llenoDe a1 d2
llenoDe a1 (Bifurcacion (Just a2) d1 d2) = a1 == a2 && llenoDe a1 d1 && llenoDe a1 d2

h1 = Habitacion 1
h2 = Habitacion 2
h3 = Habitacion 3
pj1 = Pasaje (Just 2) h1
b1 = Bifurcacion (Just 3) h2 pj1
b2 = Bifurcacion (Nothing) h3 b1
