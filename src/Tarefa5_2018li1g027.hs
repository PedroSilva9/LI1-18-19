{-|
Module      : Main
Description : Modulo integrante da Tarefa 5 do projeto a desenvolver no âmbito da unidade curricular LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>

= Introdução

Este é o módulo principal da tarefa 5. Esta tarefa trata-se de criar toda a parte gráfica do jogo recorrendo à biblioteca Gloss. Desta forma todas as outras tarefas são usadas para criar funções fundamentais para esta tarefa.

= Objetivo

Neste jogo é possível jogar com 1 - 4 jogadores sendo que o número de jogadores escolhido no menu é apenas referente aos tanques que são controlados pelo utilizadores, os restantes serão bots. Para além disto uma das novas funcionalidades é possibilidade do jogador poder apanhar powerups capazes
de aumentar o número de vidas a um tanque, aumentar o número de munições ou dar teleport  para uma posição aleatória no mapa. 

Esta tarefa relaciona os seguintes ficheiros cuja documentação pode ser vista nos seguintes links:

- DesenhaJogo:  'DesenhaJogo.hs'
- ReageEvento:  'ReageEvento.hs'
- Typos:        'Typos.hs'
- MapasDefault: 'MapasDefault.hs'
- Powerups:     'Powerups.hs'
- EstadoInicial:'EstadoInicial.hs'


= Conclusão

Assim, com a realização desta tarefa, foi possível criar uma interface gráfica onde se pode jogar o jogo que até agora vinha sendo desenvolvido. Esta tarefa também permitiu estabelecer uma ligação entre todos
as outras tarefas, permitindo visualizar todo o trabalho feito até então.

Esta tarefa também permitiu a aprendizagem de como utilizar a biblioteca gloss de forma intensiva levando a criar uma interface visualmente apelativa e obter um jogo estável. Apesar de todos os problemas enfrentados e erros obtidos, 
foi possível ultrapassá-los obtendo maior conhecimento tanto na linguagem como na biblioteca gloss.

Todos os objetivos para esta tarefa foram cumpridos, sendo que, existem algumas partes que poderiam ficar um pouco melhor desenvolvidas no entanto achamos que o resultado final é condizente com o que esperavamos.
-}
module Main where

import LI11819
import Typos
import Graphics.Gloss
import Data.List ((\\))
import Data.Maybe (isJust,fromJust)
import Powerups (generatePowerUp,fetchPowerUps)
import ReageEvento (reageEvento)
import DesenhaJogo (desenhaEstado,posVazia)
import EstadoInicial (estadoI)
import Tarefa2_2018li1g027 (jogada)
import Tarefa4_2018li1g027 (tick,groupN,asSqr)
import Tarefa6_2018li1g027 (bot)
import Tarefa1_2018li1g027 (mapaInicial)

{- | Função que controla o que acontece em todas as partes do programa ao longo do tempo.

Nesta utiliza-se a função 'tick' da tarefa 4 ('Tarefa4_20181li1g027.hs') aplicada ao estado do 'EstadoGloss' alterando-o.

> newEstado = tick . estado $ e

Posteriormente é criada a lista de posições vazias do mapa e a lista da posição dos powerups:

@
 allPos      = [(l,c) | l <- [0 .. pred . length . mapaEstado $ newEstado], c <- [0 .. pred . length . head . mapaEstado $ newEstado]]
 allPosGroup = groupN allPos $ length . head . mapaEstado $ newEstado
 vazias      = posVazia (mapaEstado newEstado) allPosGroup
 puPos       = map posicaoPowerUp $ powerups e
@

De seguida aplica-se as jogadas dos bots, obtendo-se os indices dos bots através do parâmetros 'botIndex' do 'EstadoGloss', sendo que a função 'bot' pode ser encontrada na tarefa 6.

> (botind, botPlay) = (botIndex e,map (flip bot newEstado) botind)

Deste modo, cria-se um __estadoGloss__ novo com o estado alterado pelas jogadas dos bots e os ticks. O parâmetro 'empty' é as posições vazias retirando as posições dos powerups e as posições dos bots depois de se aplicar a função 'asSqr'.

> newEstadoGloss = e{estado = newBotEst, empty = (vazias \\ puPos) \\ concatMap asSqr playersPos
}
O parâmetro 'ticks' aumenta em uma unidade e define-se uma variável winner para o segundo elementos do tuplo devolvido plea função 'checkVitoria' aplicada ao 'EstadoGloss'.

@
  newticks = succ . tickCounter $ e
  winPlr   = snd $ checkVitoria fetch{tickCounter = newticks}
@

Já na função principal se for encontrado um vencedor e o utilizador nao estiver no menu nem no criador de mapas então devolve um novo 'EstadoGloss' com 'menu' com valor True, o indice do vencedor a imagem do 'tMenu' correspondente ao índice da imagem
do jogador vencedor que está guardada no parâmetro 'imagensMenu'.

@
 if fst (checkVitoria fetch{tickCounter = newticks}) && ((not . menu $ e) && (not . criaMapa $ e))
    then e{menu = True, winner = winPlr, tMenu = (imagensMenu . imagens $ e) !! (winPlr + 6)}
    else fetch{tickCounter = newticks}  
@

-}
reageTempo :: Float -> EstadoGloss -> EstadoGloss
reageTempo n e = if victoryOrTie && ((not . menu $ e) && (not . criaMapa $ e))
                    then if winPlr == 5 
                            then e{menu = True, winner = winPlr, tMenu = (imagensMenu . imagens $ e) !! 16} 
                            else e{menu = True, winner = winPlr, tMenu = (imagensMenu . imagens $ e) !! (winPlr + 6)}
                    else fetch{tickCounter = newticks}  
    where
      newEstado             = tick . estado $ e
      allPos                = [(l,c) | l <- [0 .. pred . length . mapaEstado $ newEstado], c <- [0 .. pred . length . head . mapaEstado $ newEstado]]
      allPosGroup           = groupN allPos $ length . head . mapaEstado $ newEstado
      vazias                = posVazia (mapaEstado newEstado) allPosGroup
      puPos                 = map posicaoPowerUp $ powerups e
      (players, playersPos) = (jogadoresEstado . estado $ e, map posicaoJogador players)
      (botind, botPlay)     = (botIndex e,map (flip bot newEstado) botind)
      newBotEst             = applyPlay (zip botPlay botind) newEstado
      newEstadoGloss        = e{estado = newBotEst, empty = (vazias \\ puPos) \\ concatMap asSqr playersPos}
      generate              = if (null . empty $ newEstadoGloss) || menu e || criaMapa e then newEstadoGloss else generatePowerUp newEstadoGloss
      fetch                 = if (null . powerups $ generate)    || menu e || criaMapa e then generate else fetchPowerUps generate
      newticks              = succ . tickCounter $ e
      victoryOrTie          = fst $ checkVitoria fetch{tickCounter = newticks}
      winPlr                = snd $ checkVitoria fetch{tickCounter = newticks}

{- | Função que aplica uma lista de 'Jogada' a um 'Estado' e devolve o 'Estado' alterado.

    A lista de jogadas é filtrada pelas funções que não retornaram Nothing e passada para a função auxiliar que utiliza a função jogada da tarefa 2 usando o indice do jogador e o 'Estado'.

=== Exemplo de utilização:
>>> let e = Estado (mapaInicial (5,5)) [Jogador (1,1) B 5 5 5] []
    applyPlay [(Just (Movimenta B),0),(Just (Dispara Canhao),0)] e
    Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (2,1), direcaoJogador = B, vidasJogador = 5, lasersJogador = 5, choquesJogador = 5}], disparosEstado = [DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (3,1), direcaoDisparo = B}]}
-}
applyPlay :: [(Maybe Jogada,Int)] -> Estado -> Estado
applyPlay [] e = e
applyPlay l e = aux possiblePlays e 
  where
    possiblePlays = filter (isJust . fst) l
    aux :: [(Maybe Jogada,Int)] -> Estado -> Estado
    aux [] e = e 
    aux ((jog,ind):t) e = aux t (jogada ind (fromJust jog) e)   
{- | Função que verifica se num determinado 'EstadoGloss' existe um jogador vencedor, ou seja, se só existe um vivo e devolve um tuplo em que o primeiro elemento é True ou False dependo se existe ou não vencedor e o segundo elemento é o índice no caso de existir jogador,
sendo que, no caso de não existir permanece o índice default que é 0 (que apesar de ser o índice de um jogador este só é usado se o primeir elemento do tuplo for True).

A função guarda a lista de jogadores numa variável e usa a função 'filter' para obter a lista de jogador __com 0 vidas__. Se o tamanho desta lista for igual a 3 então é porque há um vencendor.

O indice do jogador vencedor é obtido através da função 'CheckWhoWins' que recebe a lista de vidas e o valor 0 e retorna o índice do jogador vencedor.

@
 checkWhoWins :: [Int] -> Int -> Int
 checkWhoWins [] ind = ind
 checkWhoWins (h:t) ind = if h > 0 then ind else checkWhoWins t (succ ind)
@
-}
checkVitoria :: EstadoGloss -> (Bool,Int)
checkVitoria e = if howManyDead == 3 then (True ,checkWhoWins vidaslist 0) else if howManyDead == 4 then (True,5) else (False, checkWhoWins vidaslist 0)
  where
    listajog    = jogadoresEstado . estado $ e
    vidaslist   = map vidasJogador listajog
    deadPlayers = filter (== 0) vidaslist
    howManyDead = length deadPlayers 
    checkWhoWins :: [Int] -> Int -> Int
    checkWhoWins [] ind = ind
    checkWhoWins (h:t) ind = if h > 0 then ind else checkWhoWins t (succ ind)

-- * Main

{- | Função principal do módulo que carrega todas as imagens e as agrupa em listas.

Como se pode observar em 'Imagens' existe 4 tipos de lista:

- 'imagensMenu' que guarda todas as imagens do menu;

- 'imagensJogo' que guarda todas as imagens relativas ao jogo, como os tanques, tiros etc...

- 'imagensPowerUps' que guarda as imagens referentes aos powerups;

- 'imagensTickCounter' que tem os números de 0 a 9 em imagem.
-}

main :: IO ()
main = do tank1         <- loadBMP "Pictures/Tanks/tank1.bmp"
          tank2         <- loadBMP "Pictures/Tanks/tank2.bmp"
          tank3         <- loadBMP "Pictures/Tanks/tank3.bmp"
          tank4         <- loadBMP "Pictures/Tanks/tank4.bmp"
          menuSM        <- loadBMP "Pictures/Menus/MenuSelectMap.bmp"
          menuStart     <- loadBMP "Pictures/Menus/MenuStart2.bmp"
          menuCM        <- loadBMP "Pictures/Menus/MenuCriaMapa.bmp"
          menuJogar     <- loadBMP "Pictures/Menus/MenuJogar.bmp"
          menuQJM       <- loadBMP "Pictures/Menus/MenuEscolhaJogador.bmp"
          menuInvalidMM <- loadBMP "Pictures/Menus/MenuInvalidMM.bmp"
          menuInvalidTA <- loadBMP "Pictures/Menus/MenuInvalidTA.bmp"
          menuFinalR1   <- loadBMP "Pictures/Menus/MenuFinalVitoriaR1.bmp"
          menuFinalR2   <- loadBMP "Pictures/Menus/MenuFinalVitoriaR2.bmp"
          menuFinalR3   <- loadBMP "Pictures/Menus/MenuFinalVitoriaR3.bmp"
          menuFinalR4   <- loadBMP "Pictures/Menus/MenuFinalVitoriaR4.bmp"
          menuFinalM1   <- loadBMP "Pictures/Menus/MenuFinalVitoriaM1.bmp"
          menuFinalM2   <- loadBMP "Pictures/Menus/MenuFinalVitoriaM2.bmp"
          menuFinalM3   <- loadBMP "Pictures/Menus/MenuFinalVitoriaM3.bmp"
          menuFinalM4   <- loadBMP "Pictures/Menus/MenuFinalVitoriaM4.bmp"
          menuDraw      <- loadBMP "Pictures/Menus/MenuDraw.bmp"
          menuDrawR     <- loadBMP "Pictures/Menus/MenuDrawR.bmp"
          infoCM        <- loadBMP "Pictures/Menus/InfoCriaMapa.bmp"
          pecaI         <- loadBMP "Pictures/Mapa/indestrutivel.bmp"
          pecaD         <- loadBMP "Pictures/Mapa/destrutivel.bmp"
          pecaV         <- loadBMP "Pictures/Mapa/PecaV.bmp"
          tiroC1        <- loadBMP "Pictures/Tanks/bullet1.bmp"
          tiroC2        <- loadBMP "Pictures/Tanks/bullet2.bmp"
          tiroC3        <- loadBMP "Pictures/Tanks/bullet3.bmp"
          tiroC4        <- loadBMP "Pictures/Tanks/bullet4.bmp"
          tiroL1        <- loadBMP "Pictures/Tanks/laser2.bmp"
          tiroL2        <- loadBMP "Pictures/Tanks/laser1.bmp"
          tiroL3        <- loadBMP "Pictures/Tanks/laser3.bmp"
          tiroL4        <- loadBMP "Pictures/Tanks/laser4.bmp"
          choque1       <- loadBMP "Pictures/Tanks/force1.bmp"
          choque2       <- loadBMP "Pictures/Tanks/force2.bmp"
          choque3       <- loadBMP "Pictures/Tanks/force3.bmp"
          choque4       <- loadBMP "Pictures/Tanks/force4.bmp"
          pUpLaser      <- loadBMP "Pictures/ImagensPowerUps/laser.bmp"
          pUpChoque     <- loadBMP "Pictures/ImagensPowerUps/Choque.bmp"
          pUpTeleport   <- loadBMP "Pictures/ImagensPowerUps/teleport.bmp"
          pUpVidas      <- loadBMP "Pictures/ImagensPowerUps/Vidas.bmp"
          zero          <- loadBMP "Pictures/Counter/zero.bmp"
          one           <- loadBMP "Pictures/Counter/one.bmp"
          two           <- loadBMP "Pictures/Counter/two.bmp"
          three         <- loadBMP "Pictures/Counter/three.bmp"
          four          <- loadBMP "Pictures/Counter/four.bmp"
          five          <- loadBMP "Pictures/Counter/five.bmp"
          six           <- loadBMP "Pictures/Counter/six.bmp"
          seven         <- loadBMP "Pictures/Counter/seven.bmp"
          eight         <- loadBMP "Pictures/Counter/eight.bmp"
          nine          <- loadBMP "Pictures/Counter/nine.bmp"
          estadoJog1    <- loadBMP "Pictures/Tanks/estadoJog1.bmp"
          estadoJog2    <- loadBMP "Pictures/Tanks/estadoJog2.bmp"
          estadoJog3    <- loadBMP "Pictures/Tanks/estadoJog3.bmp"
          estadoJog4    <- loadBMP "Pictures/Tanks/estadoJog4.bmp"
          let
            imagesMenu = [menuStart, menuCM, menuJogar, menuQJM,menuInvalidMM,menuInvalidTA,menuFinalM1,menuFinalM2,menuFinalM3,menuFinalM4,menuFinalR1,menuFinalR2,menuFinalR3,menuFinalR4,menuSM,infoCM,menuDraw,menuDrawR]
            imagesJogo = [pecaI,pecaD,pecaV,tank1,tank2,tank3,tank4,tiroC1,tiroC2,tiroC3,tiroC4,tiroL1,tiroL2,tiroL3,tiroL4,choque1,choque2,choque3,choque4,estadoJog1,estadoJog2,estadoJog3,estadoJog4]
            imagesPowerUps = [pUpLaser,pUpChoque,pUpTeleport,pUpVidas]
            imagesTickCounter = [zero,one,two,three,four,five,six,seven,eight,nine]
            estadoImagens = Imagens imagesMenu imagesJogo imagesPowerUps imagesTickCounter
          play dm 
               (withBlue 0.3 blue)
               fr
               (estadoI estadoImagens)
               desenhaEstado
               reageEvento
               reageTempo

-- | Dimensão nome e posição do display do jogo no ecrã.
dm ::Display
dm = InWindow "Tanks" (1280,720) (300,200)

-- | Frame rate do jogo
fr :: Int
fr = 30