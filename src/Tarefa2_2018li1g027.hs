{-|
Module      : Tarefa2_2018li1g027
Description : Tarefa 2 do projeto a desenvolver no âmbito da disciplina de LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>
Tarefa 2 do projeto a desenvolver no âmbito da unidade curricular LI1.
O objetivo desta tarefa é executar uma jogada.
-}
module Tarefa2_2018li1g027 where

import LI11819
import Tarefa1_2018li1g027 (Vetor,direcaoParaVetor)
import Data.List (nub)
import TestesT2 (teste)

-- | Dados relativos à jogada atual.
data JAtual = JAtual
  { player :: Jogador -- ^ 'Jogador' que executa a jogada.
  , playerPos :: PosicaoGrelha -- ^ 'PosicaoGrelha' do jogador que executa a jogada.
  , playerDir :: Direcao -- ^ 'Direcao' do jogador que executa a jogada.
  , playerIndex :: Int -- ^ Índice do jogador que executa a jogada.
  , othersPlayersPos :: [PosicaoGrelha] -- ^ 'PosicaoGrelha' de todos os jogadores, excepto o jogador que executou a jogada.
  , mapaAtual :: Mapa -- ^ 'Mapa' do jogo.
  , posChoque :: [PosicaoGrelha] -- ^ Conjunto de 'PosicaoGrelha' afetadas pelo choque.
  }

-- * Conjunto de testes
-- | Testes da tarefa 1. /Para ver exemplos de testes aceder ao ficheiro 'TestesT1'./
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = teste 

-- | Valor dos ticks dos disparos
ticks :: Ticks
ticks = 5

{- | Calcula a soma de dois vetores.

== Exemplo de utilização:
>>> somaVetores (1,1) (2,2)
   (3,3)
-}
somaVetores :: Vetor -> Vetor -> Vetor 
somaVetores (l1,c1) (l2,c2)= (l1+l2,c1+c2) -- Soma dois vetores

{- | Função que recebendo uma posição calcula, dependendo da direção, as posições com mais interesse para o cálculo das posições quando é efetuada a jogada.

== Exemplo de utilização:
>>> posGrelhaParaPos (1,1) B
   [(1,1),(1,2)]

== Exemplo de utilização:
>>> posGrelhaParaPos (1,1) D
   [(1,1),(2,1)]
-}
posGrelhaParaPos :: PosicaoGrelha -> Direcao -> [Posicao]
posGrelhaParaPos (li,co) dir | dir == C || dir == B = [(li,co), (li, succ co)]
                             | otherwise = [(li,co), (succ li, co)]
{- | Função que recebendo um conjunto de posições calcula, dependendo da direção, as novas posições dependendo da direção do movimento da jogada.

== Exemplo de utilização:
>>> nextMovePos [(1,1),(1,2)] C
   [(0,1),(0,2)]

== Exemplo de utilização:
>>> nextMovePos [(1,1),(1,2)] B
   [(3,1),(3,2)]

-}
nextMovePos :: [Posicao] -> Direcao -> [Posicao]
nextMovePos l dir | dir == C || dir == E = map (somaVetores $ direcaoParaVetor dir) l
                  | otherwise            = map (somaVetores $ (\(x,y) -> (2*x,2*y)) $ direcaoParaVetor dir) l

{- | Função que verifica se um conjunto de posições de um mapa são do tipo __Vazia__.

== Exemplo de utilização:
>>> let mapa = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
>>> isEmpty mapa [(3,1),(3,2)]
    [True,True]
-}   
isEmpty :: Mapa -> [Posicao] -> [Bool]
isEmpty m pos = map (f m) pos
  where
    f :: Mapa -> Posicao -> Bool
    f mapa (li, co) = Vazia == (flip (!!) co $ head . drop li $ mapa)

{- | Função que verifica se é possível mover o tanque. Esta função utiliza outras funções, tais como, 'posGrelhaParaPos', 'nextMovePos' e 'isEmpty'.

== Exemplo de utilização:
>>> let mapa = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
>>> canMove mapa (1,1) B
    True
-}
canMove :: Mapa -> PosicaoGrelha -> Direcao -> Bool
canMove m pos dir = all id listaBool 
  where
    posicoesIniciais = posGrelhaParaPos pos dir
    posicoesFinais = nextMovePos posicoesIniciais dir
    listaBool = isEmpty m posicoesFinais

{- | Movimenta um jogador num mapa.

== Exemplo de utilização:
>>> let mapa = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
>>> let jogador = Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}
>>> movimenta B mapa jogador []
    Jogador {posicaoJogador = (2,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}
-}
movimenta :: Direcao -> Mapa -> Jogador -> [PosicaoGrelha] -> Jogador
movimenta dir m (Jogador pos dirj v l c) allPlayerPos | dir /= dirj = Jogador pos dir v l c
                                                      | otherwise = if (canMove m pos dirj) && (not isChoqueBool)
                                                                    then Jogador newP dir v l c
                                                                    else Jogador pos dir v l c
  where
    newP = somaVetores pos $ direcaoParaVetor dir
    asSqr = (\(a,b) -> [(a,b),(a+1,b),(a,b+1),(a+1,b+1)])
    tankAsSqr = asSqr newP
    allpPos = nub . concat $ map asSqr allPlayerPos
    isChoque = f tankAsSqr allpPos
    isChoqueBool = any id isChoque
    f :: (Eq a) => [a] -> [a] -> [Bool]
    f [] _ = [False]
    f (h:t) l2 = elem h l2 : f t l2 

{- | Muda o estado de um jogador no estado do jogo.

== Exemplo de utilização:
>>> let estado = Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (5,1), direcaoJogador = D, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5}]}
>>> let jogador = Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}
>>> replacePlayerEstado estado jogador 1
    Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5}]}
-}
replacePlayerEstado :: Estado -> Jogador -> Int -> Estado
replacePlayerEstado e@Estado{jogadoresEstado=playersEstado} player indice = let newPlayersEstado = replace indice player playersEstado
                                                                            in e{jogadoresEstado = newPlayersEstado}

{- | Muda a direção de um jogador no estado do jogo.

== Exemplo de utilização:
>>> let estado = Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (5,1), direcaoJogador = D, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5}]}
>>> let jogador = Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}
>>> replaceDirEstado estado jogador 1 D
    Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = D, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (5,1), direcaoJogador = D, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5}]}
-}
replaceDirEstado :: Estado -> Jogador -> Int -> Direcao -> Estado
replaceDirEstado e@Estado{jogadoresEstado=playerEstado} (Jogador p d v l c) indice dir = let newDirPlayer = (Jogador p dir v l c)
                                                                                             newPlayersEstado = replace indice newDirPlayer playerEstado
                                                                                         in e{jogadoresEstado = newPlayersEstado}

{- | Muda o conteúdo de um determinado índice de uma lista.

== Exemplo de utilização:
>>> replace 0 1 [2,2,2]
    [1,2,2]
-}
replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n newVal (x:xs) | n == 0 = newVal:xs
                        | otherwise = x:replace (n-1) newVal xs

{- | Verifica se um jogador tem munições.

== Exemplo de utilização:
>>> let jogador = Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}
>>> verificaMunicao Laser jogador
    True
-}
verificaMunicao :: Arma -> Jogador -> Bool
verificaMunicao weapon player | weapon == Laser = lasersJogador player > 0
                              | otherwise = choquesJogador player > 0

{- | Remove uma munição de um jogador dependendo da arma que foi disparada.

== Exemplo de utilização:
>>> let jogador = Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}
>>> removeMunicao Laser jogador
    Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 0, choquesJogador = 1}
-}
removeMunicao :: Arma -> Jogador -> Jogador
removeMunicao Choque j@Jogador{choquesJogador=x} = let newChoquesJ = pred x
                                                    in j{choquesJogador=newChoquesJ}
removeMunicao _ j@Jogador{lasersJogador=y} = let newLaserJ = pred y
                                              in j{lasersJogador=newLaserJ}

{- | Adiciona um disparo à lista de disparos de um estado.

== Exemplo de utilização:
>>> let estado = Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (5,1), direcaoJogador = D, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5}]}
>>> addDisparo Canhao estado 0 (2,2) D
    Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (5,1), direcaoJogador = D, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (2,3), direcaoDisparo = D},DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5}]}
-}
addDisparo :: Arma -> Estado -> Int -> PosicaoGrelha -> Direcao -> Estado
addDisparo Canhao e@Estado{disparosEstado=disp} pIndice pPos pDir = let shotPos = somaVetores pPos $ direcaoParaVetor pDir
                                                                        newlistaDisparos = (DisparoCanhao pIndice shotPos pDir) : disp 
                                                                     in e{disparosEstado = newlistaDisparos}
addDisparo Laser e@Estado{disparosEstado=disp} pIndice pPos pDir  = let shotPos = somaVetores pPos $ direcaoParaVetor pDir
                                                                        newlistaDisparos = (DisparoLaser pIndice shotPos pDir) : disp
                                                                     in e{disparosEstado = newlistaDisparos}
addDisparo Choque e@Estado{disparosEstado=disp} pIndice _ _       = let newlistaDisparos = (DisparoChoque pIndice ticks) : disp
                                                                     in e{disparosEstado = newlistaDisparos}

{- | Calcula todas as posições dos jogadores que emitiram choque.

== Exemplo de utilização:
>>> let jogador = [Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}]
>>> posicaoJogadorChoque jogador [0]
    [(1,1)]
-}
posicaoJogadorChoque :: [Jogador] -> [Int] -> [PosicaoGrelha]
posicaoJogadorChoque player indices = let jogadoresChoque = map ((!!) player) indices
                                       in map posicaoJogador jogadoresChoque
{- | Calcula o índice de todos os jogadores que emitiram um choque.

== Exemplo de utilização:
>>> let disparos = [DisparoChoque {jogadorDisparo = 2, tempoDisparo = 5},DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5},DisparoLaser {jogadorDisparo = 0, posicaoDisparo = (8,6), direcaoDisparo = C}]
>>> indiceDisparoChoque disparos
    [2,1]
-}
indiceDisparoChoque :: [Disparo] -> [Int]
indiceDisparoChoque disparos = let choques = filter (\e -> case e of {DisparoChoque{} -> True; _ -> False}) disparos
                               in map jogadorDisparo choques  

{- | Calcula todas as posições do mapa afetadas pelo choque emitido por um jogador.

== Exemplo de utilização:
>>> let mapa = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
>>> posAfetadasPorChoques (1,1) mapa
    [(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),(2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4),(4,0),(4,1),(4,2),(4,3),(4,4)]
-}
posAfetadasPorChoques :: PosicaoGrelha -> Mapa -> [PosicaoGrelha]
posAfetadasPorChoques (li,co) mapa = [(l,c) | l <- [linhaI..linhaF], c <- [colunaI..colunaF]]
  where
    linhaI  = if (li - 3) < 0 then 0 else li -3
    linhaF  = if (li + 3) < (length mapa) - 1 then li +3 else pred $ length mapa
    colunaI = if (co - 3) < 0 then 0 else co -3
    colunaF = if (co + 3) < (length $ head mapa) -1 then co +3 else pred $ length $ head mapa

-- | Cria uma __JAtual__
toJAtual :: Int -> Estado -> JAtual
toJAtual ind est = JAtual player pPos pDir ind allPlayerPos mapa afectedPos
  where
    (mapa, jogs, disps, player)  = (mapaEstado est, jogadoresEstado est, disparosEstado est, flip (!!) ind . jogadoresEstado $ est)
    (pPos,pDir)                  = (posicaoJogador player, direcaoJogador player)
    comVida                      = filter (\j -> vidasJogador j /= 0) jogs
    allPlayerPos                 = filter (/= pPos) $ map posicaoJogador comVida
    choquePosList                = posicaoJogadorChoque jogs . filter (/=ind) . indiceDisparoChoque $ disps --posicao dos jogadores que emitiram choque
    afectedPos                   = nub . concat $ map (flip posAfetadasPorChoques mapa) choquePosList --todas as posições afetadas por choque num mapa

-- | Função auxiliar da função jogada que devolve um estado ou uma JAtual dependendo do número de vidas do jogador.
jogar :: Int
       -> Estado
       -> Either Estado JAtual
jogar ind est = if ((vidasJogador. flip (!!) ind . jogadoresEstado) est <= 0)
                   then Left est
                   else Right $ toJAtual ind est

-- | Função que executa uma jogada
executaJogada :: Jogada -> Estado -> JAtual -> Estado
executaJogada (Movimenta dir) est j = if elem (playerPos j) (posChoque j)
                                         then if (dir == playerDir j)
                                                 then est
                                                 else replaceDirEstado est (player j) (playerIndex j) dir
                                         else replacePlayerEstado est (movimenta dir (mapaAtual j) (player j) (othersPlayersPos j)) (playerIndex j)
executaJogada (Dispara arma) est j 
  | arma == Laser = let newPlayerEst = replacePlayerEstado est (removeMunicao arma (player j)) (playerIndex j)
                        estadoAfterShot = addDisparo arma newPlayerEst (playerIndex j) (playerPos j) (playerDir j)
                    in if verificaMunicao arma (player j)
                          then estadoAfterShot
                          else est
  | arma == Canhao = addDisparo arma est (playerIndex j) (playerPos j) (playerDir j)
  | arma == Choque = let newPlayerEst = replacePlayerEstado est (removeMunicao arma (player j)) (playerIndex j)
                         newEstado =  addDisparo arma newPlayerEst (playerIndex j) (playerPos j) (playerDir j)
                     in if verificaMunicao arma (player j)
                           then newEstado
                           else est

-- | Efetua uma jogada.
{- | Função principal da tarefa 2, que executa uma jogada.

== Exemplo de utilização:
>>> let estado = Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (3,9), direcaoJogador = B, vidasJogador = 1, lasersJogador = 0, choquesJogador = 1},Jogador {posicaoJogador = (9,6), direcaoJogador = C, vidasJogador = 3, lasersJogador = 5, choquesJogador = 1},Jogador {posicaoJogador = (5,4), direcaoJogador = E, vidasJogador = 5, lasersJogador = 2, choquesJogador = 3},Jogador {posicaoJogador = (10,6), direcaoJogador = D, vidasJogador = 2, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5},DisparoCanhao {jogadorDisparo = 1, posicaoDisparo = (6,8), direcaoDisparo = C},DisparoLaser {jogadorDisparo = 1, posicaoDisparo = (5,7), direcaoDisparo = E}]}
>>> jogada 0 (Move D) estado
    Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (3,9), direcaoJogador = D, vidasJogador = 1, lasersJogador = 0, choquesJogador = 1},Jogador {posicaoJogador = (9,6), direcaoJogador = C, vidasJogador = 3, lasersJogador = 5, choquesJogador = 1},Jogador {posicaoJogador = (5,4), direcaoJogador = E, vidasJogador = 5, lasersJogador = 2, choquesJogador = 3},Jogador {posicaoJogador = (10,6), direcaoJogador = D, vidasJogador = 2, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5},DisparoCanhao {jogadorDisparo = 1, posicaoDisparo = (6,8), direcaoDisparo = C},DisparoLaser {jogadorDisparo = 1, posicaoDisparo = (5,7), direcaoDisparo = E}]}
-}
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada ind jog est = either id (executaJogada jog est) $ jogar ind est
