{-|
Module      : Typos
Description : Modulo integrante da Tarefa 5 do projeto a desenvolver no âmbito da unidade curricular LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>
Este módulo contém todos os data Types usados na tarefa 5.
-}
module Typos where

import LI11819
import Graphics.Gloss.Interface.Pure.Game (Picture)
import System.Random (StdGen)

-- | Data type relativa a um novo estado criado nesta tarefa
data EstadoGloss = EstadoGloss
    { estado         :: Estado          -- ^Estado do jogo 
    , editor         :: Editor          -- ^Editor do jogo usado no cria mapa
    , menu           :: Bool            -- ^Verifica se está no menu ou dentro do jogo
    , imagens        :: Imagens         -- ^Imagens usadas em todas as fases do jogo
    , nJog           :: Int             -- ^Numero de jogadores
    , tMenu          :: Picture         -- ^Picture do menu em que está
    , acoesMapa      :: [Instrucao]     -- ^Mundancas que ocorrem na construcao do mapa
    , criaMapa       :: Bool            -- ^Verifica se está ou não na fase de criar um mapa
    , powerups       :: [Powerup]       -- ^Conjunto dos powerups presentes no mapa
    , powerUpCounter :: Int             -- ^Tempo de intervalo entre a geração de powerups
    , rnd            :: StdGen          -- ^Semente usada para gerar números aleatórios
    , empty          :: [PosicaoGrelha] -- ^Posições do mapa vazias
    , tickCounter    :: Int             -- ^Conjunto de caracteres no contador de ticks
    , windowSize     :: (Int,Int)       -- ^Tamanho da tela em pixeis
    , botIndex       :: [Int]           -- ^Indice dos jogadores que são bot
    , winner         :: Int             -- ^Indice do jogador vencedor (único sobrevivente)
    }
-- | Data type criado para as imagens
data Imagens = Imagens
    { imagensMenu        :: [Picture]   -- ^Lista de pictures que contém todas as imagens dos menus 
    , imagensJogo        :: [Picture]   -- ^Lista de pictures que contém todas as imagens do jogo como por exemplo, tanques, tiros etc
    , imagensPowerUps    :: [Picture]   -- ^Lista de pictures que contém as imagens dos powerups
    , imagensTickCounter :: [Picture]   -- ^Lista de pictures que imagens de números de 0 a 9 usados no mostrador de tiques ou no numero de munições dos estado dos jogadores
    }
-- | Data type relativo aos powerups
data Powerup
    = PowerupVida
        { posicaoPowerUp :: PosicaoGrelha } -- ^Posicao do powerup da vida
    | PowerupTeleport
        { posicaoPowerUp :: PosicaoGrelha } -- ^Posicao do powerup teleport
    | PowerupLasers
        { posicaoPowerUp :: PosicaoGrelha } -- ^Posicao do powerup Laser
    | PowerupChoques
        { posicaoPowerUp :: PosicaoGrelha } -- ^Posicao do powerup Choques
  deriving (Read,Show,Eq)