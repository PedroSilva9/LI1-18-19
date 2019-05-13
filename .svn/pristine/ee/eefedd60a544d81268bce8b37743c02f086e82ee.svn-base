{-|
Module      : Matrizes
Description : Ficheiro que contém as matrizes booleanas necessárias para a representação dos tetróminos.
Copyright   : José Pedro Silva <a84577@uminho.pt>; Rui Carvalho <a89498@uminho.pt>
-}
module Matrizes where

-- | Representação de uma matriz como uma lista de listas.
type Matriz a = [[a]]

-- | Matriz que representa o tetrómino I.
{- | 
    @
    [[False,True,False,False],[False,True,False,False],[False,True,False,False],[False,True,False,False]]
    @
-}
matrizI :: Matriz Bool
matrizI = replicate 4 [False, True, False, False]

-- | Matriz que representa o tetrómino J.
{- | 
    @
    [[False,True, False],[False,True, False],[True,True, False]]
    @
-}
matrizJ :: Matriz Bool
matrizJ = [[False,True, False],
           [False,True, False],
           [True,True, False]]

-- | Matriz que representa o tetrómino L.
{- | 
    @
    [[False,True, False],[False,True, False],[False,True, True]]
    @
-}
matrizL :: Matriz Bool
matrizL = [[False,True, False], 
           [False,True, False],
           [False,True, True]]

-- | Matriz que representa o tetrómino O.
{- | 
    @
    [[True, True],[True, True]]
    @
-}
matrizO :: Matriz Bool 
matrizO = replicate 2 [True, True]

-- | Matriz que representa o tetrómino Z.
{- | 
    @
    [[True,True,False],[False,True,True],[False,False,False]]
    @
-}
matrizZ :: Matriz Bool
matrizZ = [[True,True,False],
           [False,True,True],
           [False,False,False]]

-- | Matriz que representa o tetrómino T.
{- | 
    @
    [[False,False,False],[True,True,True],[False,True,False]]
    @
-}
matrizT :: Matriz Bool
matrizT = [[False,False,False],
           [True,True,True],
           [False,True,False]]

-- | Matriz que representa o tetrómino S.
{- | 
    @
    [[False,True,True],[True,True,False],[False,False,False]]
    @
-}
matrizS :: Matriz Bool
matrizS = [[False,True,True],
           [True,True,False],
           [False,False,False]]

-- | Lista de todas as matrizes
{- | 
    @
    [matrizI ,matrizJ, matrizL ,matrizO ,matrizS ,matrizT ,matrizZ]
    @
-}
allM :: [Matriz Bool]
allM = [matrizI ,matrizJ, matrizL ,matrizO ,matrizS ,matrizT ,matrizZ]
