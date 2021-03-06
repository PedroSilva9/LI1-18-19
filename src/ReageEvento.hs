{-|
Module      : ReageEvento
Description : Modulo integrante da Tarefa 5 do projeto a desenvolver no âmbito da unidade curricular LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>
Este módulo contém todos os reage evento que faz com que ao clicar-se numa tecla aconteça algo no jogo. Assim existem 3 tipos de reageEvento,
o reageEventoMenu, que corresponde a todos os eventos que acontecem no menu, o reageEventoCriaMapa que faz o mesmo mas na parte de criar um mapa personalizado e por fim
o reageEventoJogo, que controla o jogo.
-}

module ReageEvento where

import LI11819
import Typos
import EstadoInicial (estadoI)
import MapasDefault (defaultMaps)
import DesenhaJogo (checkPosVazia,posTank,posOndeCabemTanques)
import Tarefa1_2018li1g027 (constroi,instrucao,editorInicial)
import Tarefa2_2018li1g027 (jogada)
import Tarefa4_2018li1g027 (asSqr)
import Data.Maybe (fromJust)
import Data.List (elemIndex,nub,(\\))
import System.Random (mkStdGen)
import Graphics.Gloss.Interface.Pure.Game


{- | Função que recebe um evento e um 'EstadoGloss'. Apartir deste retira o menu e o CriaMapa e apartir do seu valor de verdadeiro ou falso chama a funcao reageEvento correspondente.

Se o CriaMapa tem valor True e o menu False então a função chama o 'reageEventoCriaMapa'.

Se o CriaMapa tem valor False e o menu True então, se existe algum vencedor (se o índice do winner no 'EstadoGloss' é maior ou igual a 0) chama a função 'reageEventoMenu' mas com a picture que corresponde à vitória do jogador correspondente ao indice.
Senão apenas chama a função 'reageEventoMenu'.

Se o CriaMapa tem valor False e o menu False então a função chama o 'reageEventoJogo'.
-}

reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento f estado | menu estado     = if winner estado == 5 
                                            then reageEventoMenu f estado{tMenu = (imagensMenu . imagens $ estado) !! 16} 
                                            else if winner estado >= 0
                                                    then reageEventoMenu f estado{tMenu = (imagensMenu . imagens $ estado) !! (winner estado + 6)}
                                                    else reageEventoMenu f estado
                     | criaMapa estado = reageEventoCriaMapa f estado
                     | otherwise       = reageEventoJogo f estado


-- * Reage a eventos do menu

-- | Função Principal que passa para a função 'reageEventoMenuAux' o evento o 'EstadoGloss' a picture do 'EstadoGloss' atual e a lista de imagens guardadas no 'EstadoGloss'.
reageEventoMenu :: Event -> EstadoGloss -> EstadoGloss
reageEventoMenu event e = reageEventoMenuAux event e (tMenu e) (imagensMenu . imagens $ e)

{- | Função auxiliar da função ReageEventoMenu que de acordo com a tecla clicada muda de picture no parametro tMenu no EstadoGloss apresendo-a no ecrã 

A função verifica na lista de imagens do parametro imagens do 'EstadoGloss' atual e de acordo com o indice da picture na lista e o evento devolve a nova picture.

@
reageEventoMenuAux :: Event -> EstadoGloss -> Picture -> [Picture] -> EstadoGloss
reageEventoMenuAux (EventKey (SpecialKey KeyUp) Down _ _) e pic img = case elemIndex pic img of
                                                                        Just 1  -> e{tMenu = img !! 2}
                                                                        Just 2  -> e{tMenu = img !! 1}
                                                                        Just 4  -> e{tMenu = img !! 5}
                                                                        Just 10 -> e{tMenu = img !! 6}
                                                                        Just 11 -> e{tMenu = img !! 7}
                                                                        Just 12 -> e{tMenu = img !! 8}
                                                                        Just 13 -> e{tMenu = img !! 9}
                                                                        _ -> e
@

Um dos problemas enfrentados foi a situação em que um jogador ganhava e por isso a função ia buscar a imagem correspondente à vitória do jogador com a opção de voltar ao menu selecionada, 
uma vez que também existia a opção de /restart/ nesse menu, quando o utilizador clicava na seta para baixo selecionava a opção de /restart/ pois a verificação se um jogador ganha ou não é feita no reageTempo e como
o indice do jogador vencedor era o mesmo, a função estava sempre a devolver um 'EstadoGloss' no qual a imagem no 'tMenu' é a do jogador vencedor com a opção de voltar ao menu selecionada.

Assim, para corrigir esta situação quando o utilizador clica na seta para baixo para mudar da opção de ir para o menu para a opção de restart, o indice do jogador vencedor no 'EstadoGloss'
passa a ser -1 de forma a que já não seja invocada a imagem inicial do jogador vencedor.

@
reageEventoMenuAux (EventKey (SpecialKey KeyDown) Down _ _) e pic img = case elemIndex pic img of
                                                                          Just 1 -> e{tMenu = img !! 2}
                                                                          Just 2 -> e{tMenu = img !! 1}
                                                                          Just 5 -> e{tMenu = img !! 4}
                                                                          Just 6 -> e{tMenu = img !! 10,winner = -1}
                                                                          Just 7 -> e{tMenu = img !! 11,winner = -1}
                                                                          Just 8 -> e{tMenu = img !! 12,winner = -1}
                                                                          Just 9 -> e{tMenu = img !! 13,winner = -1}
                                                                          _ -> e

@

Utilizando a lista de imagens do menu que está no parâmetro 'Imagens' do 'EstadoGloss' e clicando na tecla /Enter/ quando o utilizador está na imagem de indice __0__ ou seja no menu de entrada, esta passa para o menu que apresenta as opções de criar um mapa
ou jogar com um mapa pré-definido.

> Just 0  -> e{tMenu = img !! 1}

Se o utlizador escolher a opção de criar um mapa personalizado então o valor do 'criaMapa' no 'EstadoGloss' passa a verdadeiro e o valor do 'menu' passa para falso, entrando por isso no criador de mapas.

> Just 1  -> e{criaMapa = True,menu = False}

Se o mapa criado pelo utilizador for inválido então a imagem atual será a de indice 4, tendo como opção assinalada, a de voltar ao menu inicial. Se o utilizador pressionar a tecla /Enter/ então o jogo volta ao menu inicial chamando o 'estadoI' passando como argumento a lista de imagens do 'EstadoGloss' atual.

> Just 4  -> estadoI $ imagens e

Por outro lado se o mapa for inválido e a opção destacada for a de tentar de novo (/Retry/) então o utilizador é levado ao criador de Mapas, sendo que a função altera o estado para colocar o 'criaMapa' verdadeiro e voltando a colocar a lista de 'acoes' e o 'editor' tal como estão no 'estadoI'.

> Just 5  -> e{criaMapa = True, menu = False,acoesMapa = [], editor = instrucao Desenha $ editorInicial []}

Se a imagem for a imagem de vitória de qualquer jogador e a opção selecionada seja a de voltar ao menu inicial, ou seja, as imagens de indice 6,7,8,9 então a função chama o 'estadoI' passando como argumento a lista de imagens do 'EstadoGloss' atual. Assim o jogo volta ao menu inicial.

@
 Just 6  -> estadoI $ imagens e
 Just 7  -> estadoI $ imagens e
 Just 8  -> estadoI $ imagens e
 Just 9  -> estadoI $ imagens e
@

Por outro lado se o jogo estiver no menu de vitória de qualquer jogador mas com a opção de dar /restart/ ao jogo em destaque e o utilizador pressionar /Enter/ então a função 'resetGame' é chamada passando como argumento o 'EstadoGloss' atual. 

@
 Just 10 -> resetGame e
 Just 11 -> resetGame e
 Just 12 -> resetGame e
 Just 13 -> resetGame e
@

Após a criação de um mapa este é testado pela função 'verifyValidMap', passando como argumento o 'EstadoGloss' e o resultado da função 'changePlrEstado' que altera a lista de jogadores do estado.

Se o mapa for válido então altera os parâmetros que e passa para o jogo senão devolve uma imagem que diz que o mapa é inválido e se o utilizador quer voltar a tentar ou voltar ao menu.
-}

reageEventoMenuAux :: Event -> EstadoGloss -> Picture -> [Picture] -> EstadoGloss
reageEventoMenuAux (EventKey (SpecialKey KeyUp) Down _ _) e pic img = case elemIndex pic img of
                                                                        Just 1  -> e{tMenu = img !! 2}
                                                                        Just 2  -> e{tMenu = img !! 1}
                                                                        Just 4  -> e{tMenu = img !! 5}
                                                                        Just 10 -> e{tMenu = img !! 6}
                                                                        Just 11 -> e{tMenu = img !! 7}
                                                                        Just 12 -> e{tMenu = img !! 8}
                                                                        Just 13 -> e{tMenu = img !! 9}
                                                                        Just 17 -> e{tMenu = img !! 16}
                                                                        _ -> e

reageEventoMenuAux (EventKey (SpecialKey KeyDown) Down _ _) e pic img = case elemIndex pic img of
                                                                          Just 1 -> e{tMenu = img !! 2}
                                                                          Just 2 -> e{tMenu = img !! 1}
                                                                          Just 5 -> e{tMenu = img !! 4}
                                                                          Just 6 -> e{tMenu = img !! 10,winner = -1}
                                                                          Just 7 -> e{tMenu = img !! 11,winner = -1}
                                                                          Just 8 -> e{tMenu = img !! 12,winner = -1}
                                                                          Just 9 -> e{tMenu = img !! 13,winner = -1}
                                                                          Just 16 -> e{tMenu = img !! 17,winner = -1}
                                                                          _ -> e

reageEventoMenuAux (EventKey (SpecialKey KeyEnter) Down _ _) e pic img = case elemIndex pic img of
                                                                           Just 1  -> e{criaMapa = True,menu = False}
                                                                           Just 2  -> e{tMenu = img !! 14}
                                                                           Just 0  -> e{tMenu = img !! 1}
                                                                           Just 4  -> estadoI $ imagens e
                                                                           Just 6  -> estadoI $ imagens e
                                                                           Just 7  -> estadoI $ imagens e
                                                                           Just 8  -> estadoI $ imagens e
                                                                           Just 9  -> estadoI $ imagens e
                                                                           Just 16 -> estadoI $ imagens e
                                                                           Just 10 -> resetGame e
                                                                           Just 11 -> resetGame e
                                                                           Just 12 -> resetGame e
                                                                           Just 13 -> resetGame e
                                                                           Just 17 -> resetGame e 
                                                                           Just 5  -> e{criaMapa = True, menu = False,acoesMapa = [], editor = instrucao Desenha $ editorInicial []}
                                                                           _ -> e
reageEventoMenuAux (EventKey key Down _ _) e pic img | elem key numeroJogador && (pic == img !! 3) = if verifyValidMap e (changePlrEstado e) then estadoValido else estadoInvalido
                                                     | elem key dMaps && (pic == img !! 14) = e{estado = Estado (constroi $ defaultMaps !! fromJust (elemIndex key dMaps)) [] [], tMenu = img !! 3,tickCounter = 0,acoesMapa = defaultMaps !! fromJust (elemIndex key dMaps)}
  where
    estadoInvalido = e{menu=True, tMenu = img !! 5}
    numeroJog      = fromJust (elemIndex key numeroJogador) + 1
    botList        = take (4 - numeroJog) [3,2,1,0]
    estadoValido   = e{estado = changePlrEstado e,menu = False,criaMapa = False,nJog = 4,tickCounter = 0,botIndex = botList,winner = -1}
reageEventoMenuAux (EventKey (Char 'h') Down _ _) e pic img = estadoI $ imagens e     
reageEventoMenuAux (EventResize newWinSize) e pic img = e{windowSize = verifyWindowSize newWinSize}
reageEventoMenuAux _ e _ _ = e
-- | Lista de teclas que selecionam a quantidade de jogadores.
numeroJogador = [Char '1',Char '2',Char '3',Char '4']
-- | Lista de teclas que selecionam o mapa pré-definido. 
dMaps         = [Char '1',Char '2',Char '3',Char '4',Char '5',Char '6']

-- * Reage a eventos do criador de mapas.

{- | Função que controla os eventos do criador de mapas. Esta função mediante a tecla que é clicada chama a função 'changeMap' passando como argumento o 'EstadoGloss' atual e a instrução correspondente a essa tecla

Quando a tecla pressionada é o /Enter/ então o mapa alterado de acordo com as instrucoes é passado para o para o parâmetro 'estado' presente no 'EstadoAtual', o 'menu' passa a verdadeiro e o 'criaMapa' e a imagem no 'tMenu' é a de escolha da quantidade de jogadores.

> reageEventoCriaMapa (EventKey (SpecialKey KeyEnter) Down _ _) e = e{estado = Estado{mapaEstado = constroi .acoesMapa $ e,jogadoresEstado = [],disparosEstado =[]}, tMenu = (imagensMenu . imagens $ e) !! 3,menu = True, criaMapa = False,tickCounter = 0}

Se a imagem for redimensionada, algo que é verificado pelo 'EventResize' então a função 'verifyWindowSize' é chamada devolvendo um tamanho da janela. A utilização deste evento é bastante útil na medida em que permite redimensionar certas /pictures/ tornando-as proporcionais ao tamanho da tela.

> reageEventoCriaMapa (EventResize newWinSize) e = e{windowSize = verifyWindowSize newWinSize}

-}
reageEventoCriaMapa :: Event -> EstadoGloss -> EstadoGloss
reageEventoCriaMapa (EventKey (SpecialKey KeyRight) Down _ _) e = changeMap e $ Move D
reageEventoCriaMapa (EventKey (SpecialKey KeyLeft) Down _ _) e  = changeMap e $ Move E
reageEventoCriaMapa (EventKey (SpecialKey KeyUp) Down _ _) e    = changeMap e $ Move C
reageEventoCriaMapa (EventKey (SpecialKey KeyDown) Down _ _) e  = changeMap e $ Move B
reageEventoCriaMapa (EventKey (Char 'd') Down _ _) e            = changeMap e Desenha
reageEventoCriaMapa (EventKey (Char 't') Down _ _) e            = changeMap e MudaTetromino
reageEventoCriaMapa (EventKey (Char 'p') Down _ _) e            = changeMap e MudaParede
reageEventoCriaMapa (EventKey (Char 'r') Down _ _) e            = changeMap e Roda
reageEventoCriaMapa (EventKey (SpecialKey KeyEnter) Down _ _) e = e{estado = Estado{mapaEstado = constroi .acoesMapa $ e,jogadoresEstado = [],disparosEstado =[]}, tMenu = (imagensMenu . imagens $ e) !! 3,menu = True, criaMapa = False,tickCounter = 0}
reageEventoCriaMapa (EventKey (Char 'h') Down _ _) e            = estadoI $ imagens e
reageEventoCriaMapa (EventResize newWinSize) e                  = e{windowSize = verifyWindowSize newWinSize}
reageEventoCriaMapa _ e = e

-- * Reage a eventos do jogo
{- | Esta função é responsável pelo movimento dos tanques e o disparar das armas clicando em determinadas teclas.

Se a tecla pressionada pertencer a uma das seguintes listas:

@
 moveB  = [Char 's' ,SpecialKey KeyDown  ,Char 'k' ,Char '5']
 moveC  = [Char 'w' ,SpecialKey KeyUp    ,Char 'i' ,Char '8']
 moveD  = [Char 'd' ,SpecialKey KeyRight ,Char 'l' ,Char '6']
 moveE  = [Char 'a' ,SpecialKey KeyLeft  ,Char 'j' ,Char '4']
 canhao = [Char '1' ,Char ','            ,Char 'o' ,Char '0']
 lasers = [Char '2' ,Char '.'            ,Char 'p' ,Char '7']
 choque = [Char '3' ,Char '-'            ,Char '+' ,Char '9']

@

Então a função 'changeJogadaEstado' é chamada passando como argumento o 'EstadoGloss' atual o indice dessa tecla na lista a que pertence e a jogada correspondente. Este indice vai corresopnder ao jogador que é selecionada para fazer a jogada. 
Senão pertencer a nenhuma das listas então devolve o mesmo 'EstadoGloss'

Da mesma forma que acontece nas funções 'reageEventoMenuAux' e 'reageEventoCriaMapa' a alteração da dimensão da tela também é registado aplicando a função 'verifyWindowSize', sendo que, esta devolve um novo tamanho da tela do jogo e guarda-o no parâmetro 'windowSize' do 'EstadoGloss'.

Pelos mesmo motivos, esta funcionalidade permite resolver certos problemas em termos do tamanho de algumas figuras e assim é possível ajustá-las ao tamanho da tela do jogo.
-}

reageEventoJogo :: Event -> EstadoGloss -> EstadoGloss
reageEventoJogo (EventKey (Char 'h') Down _ _) e = estadoI $ imagens e    
reageEventoJogo (EventKey key Down _ _) e | key `elem` moveB  = changeJogadaEstado e (fromJust $ elemIndex key moveB) (Movimenta B)
                                          | key `elem` moveC  = changeJogadaEstado e (fromJust $ elemIndex key moveC) (Movimenta C)
                                          | key `elem` moveD  = changeJogadaEstado e (fromJust $ elemIndex key moveD) (Movimenta D)
                                          | key `elem` moveE  = changeJogadaEstado e (fromJust $ elemIndex key moveE) (Movimenta E)
                                          | key `elem` canhao = changeJogadaEstado e (fromJust $ elemIndex key canhao) (Dispara Canhao)
                                          | key `elem` lasers = changeJogadaEstado e (fromJust $ elemIndex key lasers) (Dispara Laser)
                                          | key `elem` choque = changeJogadaEstado e (fromJust $ elemIndex key choque) (Dispara Choque)
                                          | otherwise = e 
reageEventoJogo(EventResize newWinSize) e = e{windowSize = verifyWindowSize newWinSize}
reageEventoJogo _ e                       = e

-- | Lista de teclas que realizam a jogada 'Movimenta B'
moveB  = [Char 's' ,SpecialKey KeyDown  ,Char 'k' ,Char '5']
-- | Lista de teclas que realizam a jogada 'Movimenta C'
moveC  = [Char 'w' ,SpecialKey KeyUp    ,Char 'i' ,Char '8']
-- | Lista de teclas que realizam a jogada 'Movimenta D'
moveD  = [Char 'd' ,SpecialKey KeyRight ,Char 'l' ,Char '6']
-- | Lista de teclas que realizam a jogada 'Movimenta E'
moveE  = [Char 'a' ,SpecialKey KeyLeft  ,Char 'j' ,Char '4']
-- | Lista de teclas que realizam a jogada 'Dispara Canhao'
canhao = [Char '1' ,Char ','            ,Char 'o' ,Char '0']
-- | Lista de teclas que realizam a jogada 'Dispara Laser'
lasers = [Char '2' ,Char '.'            ,Char 'p' ,Char '7']
-- | Lista de teclas que realizam a jogada 'Dispara Choque'
choque = [Char '3' ,Char '-'            ,Char '+' ,Char '9']
-- * Funções complementares das funções anteriores.

{- | Esta função recebe um 'EstadoGloss' e uma instrução e altera o mapa do editor de modo a que o mapa seja construido de acordo com a lista de 'acoesMapa' anterior concatenada com a nova 'instrucao' e a 'instrucao' 'Desenha', com recurso à função 'constroi'.

O 'Desenha' é usado pois permite ao tetrómino atual aparecer no ecrã. 

> newEditor = edit{mapaEditor = constroi $ acoes ++ [instrucao,Desenha]}

Por fim a 'instrucao' é a adicionada à lista de 'acoesMapa', sendo que, a função devolve um novo 'EstadoGloss' com todas estas alterações. 

> newAcoes  = acoes ++ [instrucao]

-}

changeMap :: EstadoGloss -> Instrucao -> EstadoGloss
changeMap e@EstadoGloss{editor = edit, acoesMapa = acoes} instrucao = e{editor = newEditor, acoesMapa = newAcoes}
  where
    newEditor = edit{mapaEditor = constroi $ acoes ++ [instrucao,Desenha]}
    newAcoes  = acoes ++ [instrucao]

{- | Função que permite alterar um estado de acordo com a jogada e o indice do jogador.

Se um o indice do jogador for maior que o tamanho da lista ou entao esse indice for o de um bot então o estado permanece o mesmo, pois a jogada do bot é apenas aplicada na função 'ReageTempo'.

Por outro lado se não se verificar nenhuma das condições anteriores então o estado é alterado através da função 'jogada' da tarefa 2, sendo que, é este estado que é devolvido.
-}

changeJogadaEstado :: EstadoGloss -> Int -> Jogada -> EstadoGloss
changeJogadaEstado e a jog = e{estado = newest}
  where
    est    = estado e
    botind = botIndex e
    newest = if a > (pred . length . jogadoresEstado $ est) || elem a botind
                then est
                else jogada a jog est

{- | função que permite fazer /restart/ ao jogo voltando a construir o mapa inicial, usando a função 'constroi' e as acoes do mapa guardadas no 'EstadoGloss'. 

> constroi . acoesMapa $ e

A lista é extraida do estado presente no 'EstadoGloss' atual e é usada para construir um novo estado com o mapa previamente mencionado e uma lista de disparos vazia.

> newest    = Estado (constroi . acoesMapa $ e) jogList []

Desta forma é devolvido um 'EstadoGloss' que contém o newest e certos valores são colocados de forma a serem compatíveis com o inicio do jogo.

> e{estado = newEstado, menu = False, powerups = [], powerUpCounter = 0,empty = [] , rnd = mkStdGen 5,tickCounter = 0,winner = -1}
-}

resetGame :: EstadoGloss -> EstadoGloss
resetGame e = e{estado = newEstado, menu = False, powerups = [], powerUpCounter = 0,empty = [] , rnd = mkStdGen 5,tickCounter = 0,winner = -1}
  where
    jogList   = jogadoresEstado . estado $ e
    newest    = Estado (constroi . acoesMapa $ e) jogList []
    newEstado = changePlrEstado e{estado = newest}

{- | Função que testa se um dado mapa criado é válido ou não. Esta função foi feita para os casos em que o utilizador cria mapas em que o número de posições onde cabem tanques não chega para a quantidade de tanques, ou então se vai existir alguma situação em que o tanque tenha alguma parte sobreposta a outro tanque.

Numa primeira fase é criada uma lista onde se chama a função 'checkPosVazia' passando como argumento o mapa presente no 'EstadoGloss' que vai devolver a lista de posicoes vazias do mapa. Posteriormente a esta lista aplica-se a função 'posOndeCabemTaques' onde a lista
devolvida é apenas as posicoes vazias onde o tanque de facto cabe em todas as suas 4 posições.

@
 mapa                = mapaEstado . estado $ estGloss
 emptyPosList        = posOndeCabemTanques $ checkPosVazia mapa
@

a lista de jogadores é extraída do 'Estado' e é aplicada a função 'nub' que elimina posicoes iguais da lista. Esta é uma forma de verificar se a lista após aplicada a função 'nub' tem o mesmo tamanho ou não que a anterior, podendo assim concluir que havia jogadores com posições sobrepostas e assim a função retorna False.

@
 posJog = map posicaoJogador listJog
 posJogSemRepeticoes = nub posJog
@

De facto, também é usada a função 'deleteOverlapPos' aplicada à lista de jogadores para que resolva a situção mencionada anteriormente no qual alguns tanques poderiam ter alguma das suas 4 posições sobrepostas a outro tanque.

> overlapJogPos = deleteOverlapPos posJog

A posicao de jogadores é passada como argumento para a função auxiliar que verifica se alguma das posicoes do jogadores é (0,0). Esta posicao é o que função 'calculaPosMaisProx', usada para calcular a posicao de cada tanque, devolve no caso da lista das posicoes vazias ondem cabem tanques ser vazia.
@
 aux :: [Posicao] -> Bool
 aux [] = True
 aux (h:t) = h /= (0,0) && aux t
@
-}    

verifyValidMap :: EstadoGloss -> Estado -> Bool
verifyValidMap estGloss est = aux posJog && (length posJogSemRepeticoes == length posJog) && (not overlapJogPos) 
  where
    mapa                = mapaEstado . estado $ estGloss
    emptyPosList        = posOndeCabemTanques $ checkPosVazia mapa
    listJog             = jogadoresEstado est
    posJog              = map posicaoJogador listJog
    posJogSemRepeticoes = nub posJog
    overlapJogPos       = deleteOverlapPos posJog
    aux :: [Posicao] -> Bool
    aux [] = True
    aux (h:t) = h /= (0,0) && aux t

{- | Função que testa se algum tanque tem alguma das suas 4 posições sobrepostas a outro tanque.

Com recurso à função 'any' e 'elem' a lista de posicoes dos jogadores e a posição de um dos tanques é passada à função auxiliar que usa a função 'ConcatMap' e 'asSqr' para fazer a lista com todas as 4 posicoes de cada jogador tirando o jogador que vai ser testado.

>  nposl = concatMap asSqr (posl \\ [h])

As 4 posições do jogador que não pertence à lista também são calculadas.

> asSqrPos = asSqr h 

Assim verifica-se se alguma da posição da lista nposl pertence à lista das 4 posições do tanque (asSqrPos)

> any (flip elem asSqrPos) nposl

-}
deleteOverlapPos :: [Posicao] -> Bool
deleteOverlapPos l = any (aux l) l
  where
    aux :: [Posicao] -> Posicao -> Bool
    aux posl h = any (flip elem asSqrPos) nposl
      where
        nposl = concatMap asSqr (posl \\ [h])
        asSqrPos = asSqr h

{- | Função que com um 'EstadoGloss' devolve um 'Estado' que contem o mapa e a lista de disparos do 'EstadoGloss' mas com uma nova lista de jogadores proveniente da função 'posTank' aplicada ao 'Estado'.

@
 (Estado mapa jogList dispList) = estado e
 newjogList = posTank e
 changePlrEstado e = Estado mapa newjogList dispList
@

-}
changePlrEstado :: EstadoGloss -> Estado
changePlrEstado e = Estado mapa newjogList dispList
    where
      (Estado mapa jogList dispList) = estado e
      newjogList                     = posTank e

{- | Função que recebe um tuplo e verica se a razão entre o primeiro elemento do tuplo e o segundo é superior, inferior ou então se econtra entre os valores minSize e maxSize.

@
 minSize = 1280 / 720
 maxSize = 1920 / 1080
 currentSize = fromIntegral a / fromIntegral b
@

Se o valor da razão for inferior ao minSize então o valor devolvido é (1280,720):

> currentSize <= minSize = (1280,720

Se for superio então é devolvido (1920,1080):

> currentSize >= maxSize = (1920,1080)

Se estiver dentro do intervalo então o valor devolvido é o da própria razão:

> otherwise = (a,b)

Esta função foi criada para não redimensionar certas figuras abaixo de determinadas dimensões da tela, impedindo problemas visuais que possam acontecer no jogo, como por exemplo, a sobreposição da picture do estado de cada jogador com o mapa do jogo.
-}
verifyWindowSize :: (Int,Int)-> (Int,Int)
verifyWindowSize (a,b) | currentSize <= minSize = (1280,720)
                       | currentSize >= maxSize = (1920,1080)
                       | otherwise = (a,b)
  where
    minSize = 1280 / 720
    maxSize = 1920 / 1080
    currentSize = fromIntegral a / fromIntegral b
