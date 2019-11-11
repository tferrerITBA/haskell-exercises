module Parcial2018C1
    (
        Dir(..),
        Mapa(..),
        objects,
        mapMapa,
        hasObjectAt,
        longestPath,
        objectsOfLongestPath,
        allPaths,
        foldM,
        objectsFold,
        c1, c2, c3, b1, b2, b3
    ) where

import Guia8a (append)

data Dir = Left | Right | Straight deriving (Show, Eq)
data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a) deriving (Show)

objects :: Mapa a -> [a]
objects (Cofre al) = al
objects (Nada m) = objects m
objects (Bifurcacion al m1 m2) = Guia8a.append al (Guia8a.append (objects m1) (objects m2))

mapMapa :: (a -> b) -> Mapa a -> Mapa b
mapMapa f (Cofre x) = Cofre (map f x)
mapMapa f (Nada m) = Nada (mapMapa f m)
mapMapa f (Bifurcacion x m1 m2) = Bifurcacion (map f x) (mapMapa f m1) (mapMapa f m2)

hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt f (Cofre a) [] = length (filter f a) > 0
hasObjectAt f (Nada _) [] = False
hasObjectAt f (Bifurcacion a _ _) [] = length (filter f a) > 0
hasObjectAt f (Nada m) (d:ds) = hasObjectAt f m ds
hasObjectAt f (Bifurcacion _ m1 m2) (d:ds) = if d == Parcial2018C1.Left 
    then hasObjectAt f m1 ds else hasObjectAt f m2 ds

longestPath :: Mapa a -> [Dir]
longestPath (Cofre _) = []
longestPath (Nada m) = Straight:(longestPath m)
longestPath (Bifurcacion _ m1 m2) = let (p1, p2) = (longestPath m1, longestPath m2) in
    if (length p1 >= length p2) then Parcial2018C1.Left:p1 else Parcial2018C1.Right:p2

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath m = objectsOfLongestPathAux m (longestPath m)

objectsOfLongestPathAux :: Mapa a -> [Dir] -> [a]
objectsOfLongestPathAux (Cofre a) _ = a
objectsOfLongestPathAux (Nada m) (d:ds) = objectsOfLongestPathAux m ds
objectsOfLongestPathAux (Bifurcacion a m1 m2) (d:ds) = if d == Parcial2018C1.Left
    then append a (objectsOfLongestPathAux m1 ds)
    else append a (objectsOfLongestPathAux m2 ds)

allPaths :: Mapa a -> [[Dir]]
allPaths (Cofre _) = [[]]
allPaths (Nada m) = map (append [Straight]) (allPaths m)
allPaths (Bifurcacion _ m1 m2) = append (map (append [Parcial2018C1.Left]) (allPaths m1)) (map (append [Parcial2018C1.Right]) (allPaths m2))

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM f g h (Cofre a) = f a
foldM f g h (Nada m) = g (foldM f g h m)
foldM f g h (Bifurcacion a m1 m2) = h a (foldM f g h m1) (foldM f g h m2)

-- recM :: b -> () -> Mapa a -> b

objectsFold = foldM (\x -> x) (\ans -> ans) (\x ans1 ans2 -> append x (append ans1 ans2))

c1 = Cofre [1, 2, 3]
c2 = Cofre [5]
c3 = Cofre [999]

n1 = Nada c2

b1 = Bifurcacion [4] n1 c1
b2 = Bifurcacion [4] c1 n1
b3 = Bifurcacion [8] b2 c3
