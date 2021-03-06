{-|
Module      : Tarefa6_2018li1g027
Description : Modulo integrante da Tarefa 6 do projeto a desenvolver no âmbito da unidade curricular LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>

= Introdução
  
  Este módulo contém todas as funções relativas ao bot. Nesta tarefa estabelece-mos uma relação de prioridade para certas jogadas do bot.

= Objetivos   
   
   * 1 - Verificar se esta numa zona de perigo, isto é, se pode ser afetado por um  laser ou por um canhao, caso possa, mover numa direcao que permita ao jogador estar em segurança.
   * 2 - Verificar se algum jogador esta na mesma "linha" que o jogador, e se sim, ataca o jogador.
   * 3 - Verificar se algum jogador está num raio de 3 quadrados, caso se verifique usar choque e consequentemente na próxima jogada disparar laser/canhao (se estiver alinhado com o jogador, caso não esteja mudar a direção).
   * 4 - Verificar se esta numa zona de perigo, ie, se pode ser afetado por um laser ou por um canhao, caso possa, mover numa direcao que permita ao jogador estar em segurança.
   * 5 - Calcular quantos blocos destrutiveis existem na direção do jogador, caso sejam bastantes disparar laser.
   * 6 - Caso não reuna nenhuma das condições anteriores mover numa direção de modo a aproximar-se de um jogador.

= Conclusão
  
  Após a conclusão desta tarefa é possível verificar que o bot já se consegue desviar de laser dos jogadores, atacar, apróximar-se de outros jogadores etc... 
  O sitema de prioridades implementado torna muito mais fácil fornecer ao bot a melhor jogada possível. Assim, é possível afirmar que o resultado final do bot foi bom, embora reconhecemos que o nosso objetivo principal era ter o melhor bot.
  Concluindo, apesar de alguns problemas enfrentados para fazer o bot, tais como, ele bloquear em alguns locais devido aos blocos e à situação em que se encontra, conseguimos corrigir e tornar o bot um oponente minimamente forte.
-}
module Tarefa6_2018li1g027 where

import LI11819
import Tarefa2_2018li1g027 (somaVetores,posAfetadasPorChoques)
import Tarefa4_2018li1g027 (asSqr,groupN,posIndestrutivel,allPosPath,shotAsTwoBlocks)
import Tarefa1_2018li1g027 (direcaoParaVetor, mapaInicial)
import DesenhaJogo (checkPosVazia,deleteN,calculaPosMaisProx)
import Control.Monad ((>>=))
import Data.Maybe (mapMaybe,fromJust,isNothing)
import Data.List (elemIndex,nub,(\\))


{- | Função que devolve o primeiro valor Just que apareça

=== Exemplo de utilização:
>>> Just 2 <. Just 3  <. Nothing
    Just 2

>>> Nothing <. Just 3  <. Nothing
    Just 3    
-}
(<.) :: Maybe a -> Maybe a -> Maybe a
(<.) Nothing Nothing = Nothing
(<.) Nothing (Just x) = Just x
(<.) (Just x) _ = Just x

-- * Funções principais da Tarefa 6.

{- | Função que verifica se existe algum jogador que possa atacar o bot com um laser, sendo que o bot neste caso foge.

=== Exemplo de utilização:
>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5,Jogador (1,4) E 5 5 5] []
    dangerZoneLaser (e,0)
    Just (Movimenta B)

=== Exemplo de utilização:
>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5,Jogador (1,4) B 5 5 5] []
    dangerZoneLaser (e,0)
    Nothing
-}
dangerZoneLaser :: (Estado,Int) -> Maybe Jogada
dangerZoneLaser b@(e,ind) = if not cond2 || null semInversos || null semBot
                               then Nothing
                               else Just $ Movimenta betterMove
  where
    (allPlayers, bot) = (jogadoresEstado e, allPlayers !! ind)
    botPos            = asSqr . posicaoJogador $ bot
    (pathNC,path)     = (pathCalculator b, pathNC >>= id)
    semBot            = deleteN ind allPlayers
    alivePlayers      = filter (\c -> vidasJogador c > 0) semBot
    playerswithLaser  = filter (\c -> lasersJogador c > 0) alivePlayers
    cond              = (map (flip isOnPath botPos) $ zip pathNC playerswithLaser) 
    cond2             = or cond
    lDir              = (mapMaybe (flip runAwayLaser botPos) $ zip pathNC playerswithLaser) >>= id
    semInversos       = checkPossibleMove e (posicaoJogador bot) \\ nub lDir
    betterMove        = if direcaoJogador bot `elem` semInversos then direcaoJogador bot else head semInversos

{- | Função que verifica se existe algum jogador pode atingir o bot com um 'Laser'/'Canhao'.

=== Exemplo de utilização:
>>> let pj = ([(1,1),(1,2),(2,1),(2,2)], Jogador (1,1) B 5 5 5)
    isOnPath pj []
    False
-}
isOnPath :: ([Posicao], Jogador) -> [Posicao] -> Bool
isOnPath (p,j) jPos = any (flip elem p) jPos
                               
{- | Função que verifica se o jogador poderá ser atingido pelo laser e se sim retorna as 'Direcao' contrária aquelas que são usadas para fugir.

=== Exemplo de utilização:
>>> let pj = ([(1,1),(1,2),(2,1),(2,2)], Jogador (1,1) B 5 5 5)
    runAwayLaser pj []
    Nothing
-}
runAwayLaser :: ([Posicao], Jogador) -> [Posicao] -> Maybe [Direcao]
runAwayLaser (p,j) jPos = if any (flip elem p) jPos
                               then Just [direcaoJogador j, direcaoInversa . direcaoJogador $ j]
                               else Nothing

{- | Função que recebe um estado e uma posição e verifica as direções em que o jogador se pode mover

=== Exemplo de utilização:
>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5] []
    checkPossibleMove e (1,1)
    [B,D]
-}
checkPossibleMove :: Estado -> Posicao -> [Direcao]
checkPossibleMove e botPos = availableDirs
  where
    availableDirs = aux [B,C,D,E] botPos mapa
    mapa = mapaEstado e 
    aux :: [Direcao] -> Posicao -> Mapa -> [Direcao]
    aux [] _ _ = []
    aux (h:t) pos m = if checkV (somaVetores (direcaoParaVetor h) pos) m then h : aux t pos m else aux t pos m

{- | Função que recebe uma posição e um mapa e devolve se uma tanque pode avançar para essa posição.

Como um tanque tem de ser testado nas 4 posições utiliza-se a função as 'asSqr' para testar todas as posições, enviando para a função auxiliar essa lista e o mapa.

> aux (asSqr p) m

=== Exemplo de utilização:
>>> checkV (1,1) (mapaInicial (4,4))
    True

>>> checkV (3,3) (mapaInicial (4,4))
    False
-}

checkV :: Posicao -> Mapa -> Bool
checkV p m = aux (asSqr p) m
  where
    aux :: [Posicao] -> Mapa -> Bool
    aux [] m = True
    aux ((l,c):t) m = m !! l !! c == Vazia && aux t m  

{- | Calcula a direção inversa de uma dada direção.

=== Exemplo de utilização:
>>> direcaInversa D 
    E
-}
direcaoInversa :: Direcao -> Direcao
direcaoInversa = toEnum . (flip mod 4) . (+2) . fromEnum

-- | Calcula todas as 'Posicao' que podem ser afetadas por um laser de todos os 'Jogador'.
pathCalculator :: (Estado,Int) -> [[Posicao]]
pathCalculator (e,ind) = path
  where
    allPos            = [(l,c) | l <- [0 .. pred . length . mapaEstado $ e], c <- [0 .. pred . length . head . mapaEstado $ e]]
    allPosGroup       = groupN allPos $ length . head . mapaEstado $ e
    allI              = posIndestrutivel (mapaEstado e) allPosGroup
    alivePlayers      = filter (\x -> vidasJogador x /= 0) $ jogadoresEstado e
    tuplo             = deleteN ind alivePlayers >>= return . posDir
    path              = map (flip allPosPath allI . uncurry shotAsTwoBlocks) tuplo

{- | Função que devolve um tuplo com a posição e direção de um jogador.

=== Exemplo de utilização:
>>> posDir (Jogador (1,1) D 5 5 5)
    ((1,1),D)
-}
posDir :: Jogador -> (PosicaoGrelha,Direcao)
posDir (Jogador p d _ _ _) = (p,d)

{- | Função que verifica se o caminho de algum disparo Canhao interseta a posição do bot.

Se a quantidade de disparos no 'radiusBlock' for __0__ então devolve /Nothing/.

> | qtsdispOnPath == 0 = Nothing

Se a quantidade de disparos no 'radiusBlock' for __maior que 1 __ então ou se move ou dispara um canhao dependo se o resultado da função 'bestplay' é ou não vazio e se na lista de disparos existe algum disparo canhão de outro jogador

@
 | qtsdispOnPath > 1 = if isNothing bestMove || null canhoesNoBot
                          then Just $ Dispara Canhao
                          else Just $ Movimenta $ fromJust bestMove
@

=== Exemplo de utilização:
>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5] [DisparoCanhao 1 (3,1) C]
    dangerZoneCanhoes (e,0)
    Just (Movimenta D)
-}

dangerZoneCanhoes :: (Estado, Int) -> Maybe Jogada
dangerZoneCanhoes (e,ind) | null allCanhoes = Nothing 
                          | otherwise = if isNothing bestMove
                                           then Just $ Dispara Canhao
                                           else Just $ Movimenta $ fromJust bestMove         
  where
    allCanhoes                       = filter (\d -> case d of {DisparoCanhao{} -> True; _ -> False}) $ disparosEstado e
    (allPlayers, bot)                = (jogadoresEstado e, allPlayers !! ind)
    inDanger                         = concatMap (flip radiusBlock (mapaEstado e)) (asSqr . posicaoJogador $ bot)
    (allDispPos,allDispDir)          = (map posicaoDisparo allCanhoes, map direcaoDisparo allCanhoes)
    dangerDispCanhao                 = map (flip elem inDanger) allDispPos
    (dispOnPathIndex, qtsdispOnPath) = quantosDispCanhaoPath dangerDispCanhao
    (onlyDispDir,dirInversaBot)      = (allDispDir !! head dispOnPathIndex, direcaoInversa . direcaoJogador $ bot)
    bestMove                         = bestplay dispOnPathIndex allDispDir dirInversaBot (posicaoJogador bot) e 
    botDir                           = direcaoJogador bot
{- | Função que calcula a melhor direção para o bot. 

A função calcula as direções possíveis que o bot pode seguir através da função 'checkPossibleMove' e retira dessa lista as direções inversas do tiro para que o bot não avançe para cima do tiro
e retira a direção inversa do bot para que este se mova para longe do tiro.

=== Exemplo de utilização:
>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5] [DisparoCanhao 1 (3,1) C]
    bestplay [0] [C] C (1,1) e  
    Just D
-}

bestplay :: [Int] -> [Direcao]-> Direcao -> Posicao -> Estado -> Maybe Direcao
bestplay l dispDir dirInversaBot botPos e | null availableDir = Nothing 
                                          | botDirElem && null (availableDir \\ [botdir]) = Nothing
                                          | botDirElem && (not $ null $ availableDir \\ [botdir]) =  Just $ head $ availableDir \\ [botdir]
                                          | otherwise = Just $ head availableDir
  where
    botdir            = direcaoInversa $ dirInversaBot
    alldispC          = map direcaoInversa dispDir
    botDirElem        = elem botdir alldispC
    allDispDirOnPath  = nub $ dirInversaBot : aux l dispDir
    checkemptyDir     = checkPossibleMove e botPos
    availableDir      = checkemptyDir \\ allDispDirOnPath
    aux :: [Int] -> [Direcao] -> [Direcao]
    aux t l = foldr (\h -> (:) (l !! h)) l t

{- | Devolve o indice dos disparos canhão que estão no caminho do bot e a quantidade. Assim sabe-se quais os indices que dos disparos com que o bot se deve preocupar.

=== Exemplo de utilização:
>>> quantosDispCanhaoPath [True,False,True]
    ([0,2],2)
-}    
quantosDispCanhaoPath :: [Bool] -> ([Int],Int)
quantosDispCanhaoPath l  = (aux 0 l, length $ filter (==True) l)
  where
    aux :: Int -> [Bool] -> [Int]
    aux _ [] = []
    aux a (h:t) = if h then a : aux (a+1) t else aux (a+1) t
{- | Função que devolve as posições que estão à num quadrado 4x4 à volta de uma posição. 

Assim, é possível o bot detetar todos os tiros que estão dentro desta área.

=== Exemplo de utilização:
>>> radiusBlock (2,2) (mapaInicial (5,5))
    [(0,0),(0,1),(0,2),(0,3),(0,4),(1,0),(1,1),(1,2),(1,3),(1,4),(2,0),(2,1),(2,2),(2,3),(2,4),(3,0),(3,1),(3,2),(3,3),(3,4),(4,0),(4,1),(4,2),(4,3),(4,4)]
-}
radiusBlock :: Posicao -> Mapa -> [PosicaoGrelha]
radiusBlock (li,co) mapa = [(l,c) | l <- [linhaI..linhaF], c <- [colunaI..colunaF]]
  where
    linhaI  = if (li - 2) < 0 then 0 else li -2
    linhaF  = if (li + 2) < (pred . length $ mapa) then li +2 else pred . length $ mapa
    colunaI = if (co - 2) < 0 then 0 else co -2
    colunaF = if (co + 2) < (pred .length . head $ mapa) then co +2 else pred . length . head $ mapa

{- | Função que verifica se o bot tem algum jogador dentro da área de choque dele para poder disparar o choque.

A função apenas ativa o choque se já não tiver um ativado e se a munição do choque é superior a 0

=== Exemplo de utilização:
>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5, Jogador (2,2) C 5 5 5] []
    inChoque (e,0)
    Just (Dispara Choque)

>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5, Jogador (6,6) C 5 5 5] []
    inChoque (e,0)
    Nothing
-}
inChoque :: (Estado, Int) -> Maybe Jogada
inChoque (e,ind) = if any (flip elem pos) allpPos && null choqueBot && choqueAmmo > 0
                      then Just $ Dispara Choque
                      else Nothing
  where
    (allPlayers, bot) = (jogadoresEstado e, allPlayers !! ind)
    choqueAmmo        = choquesJogador bot
    semBot            = deleteN ind allPlayers
    alivePlayers      = filter (\c -> vidasJogador c > 0) semBot
    pos               = posAfetadasPorChoques (posicaoJogador bot) (mapaEstado e)
    aliveP            = filter (\c -> vidasJogador c > 0) alivePlayers
    allpPos           = map posicaoJogador alivePlayers
    choqueBot         = filter (\d -> case d of {DisparoChoque{jogadorDisparo = ind} -> True; _ -> False}) $ disparosEstado e

{- | Função que dispara ou um laser ou um tiro canhão (se não tiver munições do laser) quando um jogador aparece no caminho do bot.

> if laserAmmo > 0 then Just $ Dispara Laser else Just $ Dispara Canhao

A função filtra a lista de jogadores sem o bot pelos jogadores que ainda estão vivos e calcula o caminho do bot até atingir um bloco indestrutivel.
Se algum dos jogadores estiver no caminho o bot ataca.

=== Exemplo de utilização:
>>>  let e = Estado (mapaInicial (10,10)) [Jogador (1,1) D 5 5 5, Jogador (1,6) C 5 5 5] []
    shootToKill (e,0)
    Just (Dispara Laser)

>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5, Jogador (1,6) C 5 5 5] []
    shootToKill (e,0)
    Nothing

>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 0 5, Jogador (6,1) C 5 5 5] []
    shootToKill (e,0)
    Just (Dispara Canhao)
-}
shootToKill :: (Estado,Int) -> Maybe Jogada
shootToKill (e,ind) = if any (flip elem allPlayersPos) path 
                         then if laserAmmo > 0 then Just $ Dispara Laser else Just $ Dispara Canhao
                         else Nothing
  where
    (allPlayers, bot) = (jogadoresEstado e, allPlayers !! ind)
    allPos            = [(l,c) | l <- [0 .. pred . length . mapaEstado $ e], c <- [0 .. pred . length . head . mapaEstado $ e]]
    allPosGroup       = groupN allPos $ length . head . mapaEstado $ e
    allI              = posIndestrutivel (mapaEstado e) allPosGroup
    path              = flip allPosPath allI $ uncurry shotAsTwoBlocks . posDir $ bot
    semBot            = deleteN ind allPlayers
    alivePlayers      = filter (\x -> vidasJogador x /= 0) $ semBot
    allPlayersPos     = concatMap (asSqr . posicaoJogador) alivePlayers
    laserAmmo         = lasersJogador bot 
{- | Função que dispara contra blocos destrutiveis se estes estiverem no caminho do bot.

Se o bot tiver 4 ou mais blocos no caminho e a munição do laser for superior a 0 então dispara um laser.

> | howManyOnPath >= 4 && laserAmmo > 0 = Just $ Dispara Laser

Se tiver algum no caminho então dispara um tiro de canhão, senão nao dispara nada 

@
 | howManyOnPath > 0 = Just $ Dispara Canhao
 | otherwise = Nothing
@


=== Mapa com 4 blocos destrutivel no caminho: 
>>> let m = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
    shootDestructable (e,0)
    Just (Dispara Laser)

=== Mesmo mapa mas sem lasers:
>>> let m = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
    shootDestructable (e,0)
    Just (Dispara Canhão)
-}
shootDestructable :: (Estado,Int) -> Maybe Jogada
shootDestructable (e,ind) | howManyOnPath >= 4 && laserAmmo > 0 = Just $ Dispara Laser
                          | howManyOnPath > 0 = Just $ Dispara Canhao
                          | otherwise = Nothing
  where 
    bot                = jogadoresEstado e !! ind
    laserAmmo          = lasersJogador bot
    (allPiecesD, path) = howManyDestructable (e, ind)
    howManyOnPath      = length allPiecesD

{- | Função que recebe um estado e um int e devolve um tuplo com a lista de posições com blocos destrutíveis e o caminho para eles.

Esta função utiliza a função 'findPieceDest' para verificar se numa posição a peça __é destrutível__ ou não, devolvendo a posição se for.

=== Exemplo de utilização (Usando o mapa do exemplo da função 'shootDestructable' :
>>> howManyDestructable (e,0) 
    ([(1,5),(1,6),(1,7),(1,8)] , [(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9)])
-}
howManyDestructable :: (Estado,Int) -> ([Posicao],[Posicao])
howManyDestructable (e,ind) = (allPiecesD, path)
  where
    (allPlayers, bot) = (jogadoresEstado e, allPlayers !! ind)
    allPos            = [(l,c) | l <- [0 .. pred . length . mapaEstado $ e], c <- [0 .. pred . length . head . mapaEstado $ e]]
    allPosGroup       = groupN allPos $ length . head . mapaEstado $ e
    allI              = posIndestrutivel (mapaEstado e) allPosGroup
    path              = flip allPosPath allI $ (uncurry shotAsTwoBlocks) . posDir $ bot 
    allPiecesD        = mapMaybe (findPieceDest (mapaEstado e)) path

{- | Função que verifica se numa dada posicao a peça é destrutível, retornando a posição se for.

=== Exemplo de utilização (Usando o mapa do exemplo da função 'shootDestructable' :
>>> findPieceDest m (4,4)
    Nothing

=== Exemplo de utilização (Usando o mapa do exemplo da função 'shootDestructable' :
>>> findPieceDest m (1,6)
    Just (1,6)
-}

findPieceDest :: Mapa -> Posicao -> Maybe Posicao
findPieceDest m p@(l,c) = if (m !! l !! c) == Bloco Destrutivel
                             then Just p
                             else Nothing
{- | Função que aproxima o bot de outro jogador.

Esta função filtra a lista dos jogadores sem o bot por aqueles que ainda estão vivos. 

@
 (allPlayers, bot) = (jogadoresEstado e, allPlayers !! ind)
 semBot            = deleteN ind allPlayers
 alivePlayers      = filter (\x -> vidasJogador x /= 0) semBot
@

Posteriormente calcula a posição do jogador e a posição do jogador mais próximo através da função 'calculaPosMaisProx'

> jogadorMaisProx   = calculaPosMaisProx botPos semBotPos 

Usando a função 'checkPossibleMove' recebe a lista das direções que o bot pode utilizar.

> availableDirs = checkPossibleMove e botPos

E usa a função 'bestDir' para calcular a lista de direções que o bot se pode mover retirando a direção inversa do bot para ele não voltar para trás.

> bestDir botPos semBotPos (availableDirs \\ [direcaoInversa $ direcaoJogador bot]) e  

Assim se esta lista for vazia a função devolve nothing, senão devolve a 'Jogada' 'Movimenta' com a primeira direção da lista 

> if null bestDirToMove then Nothing else Just . Movimenta $ (head bestDirToMove)

Assim o bot move-se para junto de um jogador onde posteriormente pode atacar ou não dependendo do resultado de outras funções 

=== Exemplo de utilização:
>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5, Jogador (1,6) C 5 5 5] []
    aproxPlayer (e,0)
    Just (Movimenta D)
-}
aproxPlayer :: (Estado,Int) -> Maybe Jogada
aproxPlayer (e,ind) = if null bestDirToMove then Nothing else Just . Movimenta $ (head bestDirToMove)
  where
    (allPlayers, bot) = (jogadoresEstado e, allPlayers !! ind)
    semBot            = deleteN ind allPlayers
    alivePlayers      = filter (\x -> vidasJogador x /= 0) semBot
    semBotPos         = map posicaoJogador alivePlayers 
    botPos            = posicaoJogador bot
    availableDirs     = checkPossibleMove e botPos
    jogadorMaisProx   = calculaPosMaisProx botPos semBotPos 
    bestDirToMove     = bestDir botPos semBotPos (availableDirs \\ [direcaoInversa $ direcaoJogador bot]) e  

{- | Função que calcula a lista com as melhores direções que o bot se pode movimentar.

A função calcula as posições possiveis do bot onde o bot pode de facto se mover.

@
  l                = map direcaoParaVetor dirList
  mapa             = mapaEstado e
  listPosVazia     = checkPosVazia mapa
  possibleBotPos   = map (somaVetores p) l
@

Posteriormente calcula o jogador mais próximo e a posição que o bot se pode mover mais próxima do jogador:

@
 jogadorMaisProx  = calculaPosMaisProx p jogPosList 
 nextPos          = calculaPosMaisProx jogadorMaisProx possibleBotPos
@

Assim se esta lista de posições for nula então o bot não se pode mover em nenhuma direção, senão devolve a direção correspondente ao índice da posição mais próxima na lista de posições.

> if null nextPos then [] else [dirList !! fromJust (elemIndex nextPos possibleBotPos)]

=== Exemplo de utilização :
>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5, Jogador (3,3) C 5 5 5] []
    bestDir (1,1) [(3,3)] [D,B] e 
    [D]
-}
    
bestDir :: Posicao -> [Posicao] -> [Direcao] -> Estado -> [Direcao]
bestDir _ _ [] _ = []
bestDir _ [] _ _ = []
bestDir p jogPosList dirList e = if null nextPos then [] else [dirList !! fromJust (elemIndex nextPos possibleBotPos)]
  where
    l                = map direcaoParaVetor dirList
    mapa             = mapaEstado e
    listPosVazia     = checkPosVazia mapa
    possibleBotPos   = map (somaVetores p) l
    jogadorMaisProx  = calculaPosMaisProx p jogPosList 
    nextPos          = calculaPosMaisProx jogadorMaisProx possibleBotPos

{- | Define um ro'bot' capaz de jogar autonomamente o jogo.

    Como mencionado anteriormente definimos uma lista de prioridades na qual se pode definir de forma simples através da função '(<.)'.

    Esta função foi criada para facilitar o processo de tomada de decisão do bot.

=== Exemplo de utilização:
>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) B 5 5 5, Jogador (3,3) C 5 5 5] []
    bot 0 e
    Just (Movimenta D)

>>> let e = Estado (mapaInicial (10,10)) [Jogador (1,1) D 5 5 5, Jogador (1,6) C 5 5 5] []
    bot 0 e
    Just (Dispara Laser)
-}
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot ind e = dangerLaser <. toKill <. dangerCanhoes <. choqueEnemies <. destructable <. getCloser 
  where
    b             = (e,ind)
    dangerLaser   = dangerZoneLaser b
    dangerCanhoes = dangerZoneCanhoes b
    choqueEnemies = inChoque b
    toKill        = shootToKill b
    destructable  = shootDestructable b
    getCloser     = aproxPlayer b