{-|
Module      : Powerups
Description : Modulo integrante da Tarefa 5 do projeto a desenvolver no âmbito da unidade curricular LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>
Este módulo contém as funções relativas aos powerups do jogo.
-}
module Powerups where

import LI11819
import Typos
import System.Random (StdGen,randomR)
import Data.List (partition,intersect,delete,(\\))
import Data.Maybe (mapMaybe)
import Tarefa4_2018li1g027 (asSqr)

-- | Função que retorna todas as posições 'Vazia's de um mapa.
posVazias :: Mapa -> [[Posicao]] -> [Posicao]
posVazias [] [] = []
posVazias (x:xs) (y:ys) = aux x y ++ posVazias xs ys
  where
    aux :: [Peca] -> [Posicao] -> [Posicao]
    aux [] [] = []
    aux (x:xs) (y:ys) = if x == Vazia then y : aux xs ys else aux xs ys

-- ** Gerar Powerups
-- | Função que gera um 'Powerup'.
generatePowerUp :: EstadoGloss -> EstadoGloss
generatePowerUp e = let oldPowerUps = powerups e
                    in if (powerUpCounter e == 0) && (not . null $ vaziasAsSqr) && all (verifiesEmpty mapa) (asSqr newPos)
                          then e{powerups = powerup : oldPowerUps, rnd = seed, powerUpCounter = 100}
                          else if powerUpCounter e < 0
                                  then e{rnd = seed, powerUpCounter = 100}
                                  else e{rnd = seed, powerUpCounter = pred . powerUpCounter $ e}
  where
    mapa          = mapaEstado . estado $ e
    vaziasAsSqr   = generatePowerUpAux (mapaEstado . estado $ e) $ empty e
    (rand, seed)  = randomR (0, pred . length $ vaziasAsSqr) $ rnd e
    newPos        = vaziasAsSqr !! rand
    powerup       = flip createPowerUp newPos $ mod rand 4

-- | Função que verifica se uma posição de um mapa é 'Vazia'.
verifiesEmpty :: Mapa -> Posicao -> Bool
verifiesEmpty m (l,c) = m !! l !! c == Vazia

{- | Função que escolhe aleatóriamente um powerup, podendo este ser 'PowerupVida','PowerupTeleport', 'PowerupLasers' ou 'PowerupChoques'.
 
== Exemplo de Utilização:
>>> createPowerUp 2 (1,1)
    PowerupLasers {posicaoPowerUp = (1,1)}
-}
createPowerUp :: Int -> PosicaoGrelha -> Powerup
createPowerUp 0 pos = PowerupVida pos
createPowerUp 1 pos = PowerupTeleport pos
createPowerUp 2 pos = PowerupLasers pos
createPowerUp 3 pos = PowerupChoques pos

-- | Função auxiliar da função 'generatePowerUp'.
generatePowerUpAux :: Mapa -> [PosicaoGrelha] -> [PosicaoGrelha]
generatePowerUpAux m p = mapMaybe (verifica m) p

-- | Função que dado um 'Mapa' uma 'Posicao' verifica se as 4 células à volta são 'Vazia's. 
verifica :: Mapa -> PosicaoGrelha -> Maybe PosicaoGrelha
verifica m p = if all (f m) asSquare
                  then Just p
                  else Nothing
  where
    asSquare = asSqr p
    f :: Mapa -> Posicao -> Bool
    f mp (l,c) = (!!) (head $ drop l mp) (pred c) == Vazia

-- ** Efeito dos powerups
-- | Função principal que aplica as mudanças necessárias num estado caso um jogador apanhe um 'Powerup'.
fetchPowerUps :: EstadoGloss -> EstadoGloss
fetchPowerUps e = if null . empty $ e then e else e{estado = newEstado, powerups = deletePU, rnd = newSeed}
  where
    (players, playersPos)               = (jogadoresEstado . estado $ e, map posicaoJogador players)
    (tp, resto)                         = partition (\e -> case e of {PowerupTeleport{} -> True; _ -> False}) $ powerups e
    (tpPos, restoPos)                   = (map posicaoPowerUp tp, map posicaoPowerUp resto)
    (intersectionTP, intersectionResto) = (tpPos `intersect` playersPos, restoPos `intersect` playersPos)
    mapa                                = mapaEstado . estado $ e
    afterTP = teleported mapa (rnd e) players intersectionTP $ generatePowerUpAux (mapaEstado . estado $ e) (empty e) \\ concatMap asSqr playersPos
    (newJTP, listSeed)                  = (map fst afterTP, map snd afterTP)
    newSeed                             = if null listSeed then rnd e else last listSeed
    (jAfterPU, deletePU)                = (aplicaPU newJTP playersPos resto, deletePowerUp (powerups e) $ intersectionTP ++ intersectionResto)
    (est, newEstado)                    = (estado e, est{jogadoresEstado = jAfterPU})
    
-- | Função que elimina do 'EstadoGloss' todos os 'Powerup's.
deletePowerUp :: [Powerup] -> [PosicaoGrelha] -> [Powerup]
deletePowerUp [] _ = []
deletePowerUp (pu:t) pos = let p = posicaoPowerUp pu
                           in if p `elem` pos
                                 then deletePowerUp t pos
                                 else pu : deletePowerUp t pos

-- | Função que caso um 'Jogador' se encontre na 'PosicaoGrelha' de um 'Powerup' aplica esse 'Powerup' no 'Jogador'.
aplicaPU :: [Jogador] -> [PosicaoGrelha] -> [Powerup] -> [Jogador]
aplicaPU j jPos pu = map (flip aplicaPUAux pu) $ zip j jPos

-- | Função auxiliar da 'aplicaPU'.
aplicaPUAux :: (Jogador,PosicaoGrelha) -> [Powerup] -> Jogador
aplicaPUAux (j,_) [] = j
aplicaPUAux jog@(j,pos) (PowerupVida p:t)     = if p == pos
                                                   then j{vidasJogador = succ . vidasJogador $ j}
                                                   else aplicaPUAux jog t
aplicaPUAux jog@(j,pos) (PowerupLasers p:t)   = if p == pos
                                                   then j{lasersJogador = succ . lasersJogador $ j}
                                                   else aplicaPUAux jog t
aplicaPUAux jog@(j,pos) (PowerupChoques p:t)  = if p == pos
                                                   then j{choquesJogador = succ . choquesJogador $ j}
                                                   else aplicaPUAux jog t

-- | Função que aplica um 'PowerupTeleport' a um 'Jogador' caso este se encontre em cima de um 'Powerup'
teleported :: Mapa -> StdGen -> [Jogador] -> [PosicaoGrelha] -> [PosicaoGrelha] -> [(Jogador,StdGen)]
teleported _ _ [] _ _ = []
teleported m seed l@(j:js) p v = if (pos `elem` p) && (not . null $ v)
                                    then (newJ, newSeed) : teleported m newSeed js p (delete newPos v)
                                    else (j,seed)        : teleported m seed js p v
  where
    pos                 = posicaoJogador j
    (rnd, newSeed)      = randomR (0, pred . length $ v) seed
    allJpos             = concatMap (asSqr . posicaoJogador) l
    (rndPos, rndPosSqr) = (v !! rnd, asSqr rndPos)
    newPos              | any (flip elem allJpos) rndPosSqr = pos
                        | all (verifiesEmpty m) rndPosSqr = rndPos
                        | otherwise = pos
    newJ                = j{posicaoJogador = newPos}