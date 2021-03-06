{-|
Module      : DesenhaJogo
Description : Modulo integrante da Tarefa 5 do projeto a desenvolver no âmbito da unidade curricular LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>
Porção de código da tarefa 5 que permite desenhar o jogo, mais precisamente, os tanques, o mapa, os tiros e toda a informação relativa ao estado dos jogadores.

Permite também desenhar o construtor de mapas no ecrã, bem como o tetrómino e todo o seu movimento e as intruções de como utilizá-lo.
-}
module DesenhaJogo where

import LI11819
import Typos
import Data.List (nub)
import Data.Char (digitToInt)
import Tarefa4_2018li1g027 (groupN,posIndestrutivel,laserPath,shotAsTwoBlocks,allPosPath)
import Tarefa1_2018li1g027
import Graphics.Gloss.Interface.Pure.Game


{- | Função que apaga um elemento num determinado índice de uma lista.

=== Exemplo de utilização:
>>> deteleN 2 [1,2,3,4] = [1,2,4]
-}

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as) | i == 0    = as   
                 | otherwise = a : deleteN (i-1) as
-- * Funções principais para desenhar o Estado.
{- | Função que mediante os valores lógicos das variáveis 'menu' e 'criaMapa' desenha imagens na tela do jogo.

  Se o valor de 'menu' do 'EstadoGloss' for verdade então o utilizador está no menu e o programa faz aparecer a imagem do menu que se encontra no 'tMenu' atual.

  Se o 'criaMapa' for verdade e o 'menu' falso então a função 'desenhaCriaMapa' passando como argumento o mapa do 'editor' e o 'EstadoGloss' atual.

  Se Nenhum dos anteriores for verdade então quer dizer que o utilizador se encontra realmente no jogo e chama a função 'desenhaJogo' passando o 'EstadoGloss' como argumento

-}
desenhaEstado :: EstadoGloss -> Picture
desenhaEstado e | menu e = tMenu e
                | criaMapa e = desenhaCriaMapa (mapaEditor . editor $ e) e
                | otherwise = desenhaJogo e

{- | Função que desenha todos os elementos do jogo, sendo que, vai buscar as diferentes pictures às funcoes correspondestes agrupando-as numa só lista e recorrendo à função Pictures forma só uma picture.

  Esta função relaciona as seguintes funções: 

* 'desenhaCriaMapa' que desenha na tela do jogo o mapa no criador de mapas e a informação relativa ao tetrómino ou só desenha o mapa se estiver no jogo.

* 'desenhaTanques' que como o nome indica desenha os tanques nas posições mais próximas dos cantos do mapa.

* 'desenhaTiro' que desenha todos os tiros sendo eles o laser, canhão e choque

* 'desenhaPowerUps' que desenha os powerups no mapa.

* 'drawTicksCounter' que faz com que apareça um contador de ticks na parte inferior da tela.

* 'getEstados' que desenha os estados dos tanques durante o jogo , tal como, as vidas, munições etc... 
-}
desenhaJogo :: EstadoGloss -> Picture
desenhaJogo e = Pictures list
  where 
    mapa    = mapaEstado . estado $ e
    picMapa = desenhaCriaMapa mapa e
    tanques = Pictures $ desenhaTanques e
    tiros   = Pictures $ desenhaTiro e
    powerUP = Pictures $ desenhaPowerUps e
    ticks   = drawTicksCounter e
    estados = getEstados e 
    list    = [picMapa,tanques,tiros,powerUP,ticks,estados]

{- | Função usada para desenhar o criador de mapas ou o mapa do jogo que recebe um 'Mapa' e um 'EstadoGloss' e devolve a picture do 'Mapa'. 

  A Função calcula o tamanho ideal de uma peça através da função 'pecaSize' e recebe a posicao do mapa enviando para a função 'desenhaMapa' estas variáveis como argumento recebendo a picture do mapa.

@
 (bSize,pixelSize) = pecaSize e m
 criaM             = criaMapa e 
 (posX,posY)       = getPecaPos e m
 getInfo           = desenhaInfoCM e
 getMapaPic        = desenhaMapa m posX posY bSize pixelSize e
@

  Posteriormente a função guarda na variável getInfo o resultado da função 'desenhaInfoCM' que é responsável por desenhar no ecrã a imagem das informações relativas ao tetrómino, ou seja, as teclas a clicar para rodá-lo, movê-lo etc...

> getInfo = desenhaInfoCM e

  Atráves do valor da variável 'criaMapa' do 'EstadoGloss' a função verifica se o utilizador está no criador de mapas ou no jogo e assim desenha o mapa e as informações do tetrómino no primeiro caso ou entao só desenha o mapa se se encontrar na segunda situação.

> if criaM then Pictures getMapAndInfo else Pictures getMapaPic 
-}


desenhaCriaMapa :: Mapa -> EstadoGloss -> Picture
desenhaCriaMapa m e = if criaM then Pictures getMapAndInfo else Pictures getMapaPic 
  where
    (bSize,pixelSize) = pecaSize e m
    criaM             = criaMapa e 
    (posX,posY)       = getPecaPos e m 
    getInfo           = desenhaInfoCM e
    getMapaPic        = desenhaMapa m posX posY bSize pixelSize e
    getMapAndInfo     = getMapaPic ++ [getInfo]

-- * Funções complementares para desenhar o criador de mapas ou desenhar o mapa do jogo.
{- | Função que desenha a informação do criador de mapas na tela do jogo.

  A informação é desenhada relativa ao tamanho da tela usando apenas o espaço do lado direito do mapa. Assim a imagem da informação é também redimensionada para se ajustar à tela do jogo.

> (fromIntegral x - 500 - realPixelSize * (fromIntegral . length . head $ mapa))/3
-}
desenhaInfoCM :: EstadoGloss -> Picture
desenhaInfoCM e = translatedPic
  where pic                    = (imagensMenu . imagens $ e) !! 15 
        mapa                   = mapaEstado . estado $ e 
        (x,y)                  = windowSize e 
        (sizePs,realPixelSize) = getPecaPos e mapa
        espaco                 = (fromIntegral x - 500 - realPixelSize * (fromIntegral . length . head $ mapa))/3
        resize                 = espaco / 1400
        sizePx                 = resize * 700
        resizePic              = Scale (resize) resize pic 
        translatedPic          = Translate (fromIntegral x/2 -sizePx/2) 0 resizePic

{- | Função que desenha o mapa na tela do jogo.

  Esta função desenha linha a linha o mapa, sendo que utiliza a função 'desenhaLinha' usando como valor para 'Scale' o melhor tamanho para a peça obtido através da função 'sizePeca'.

> pic = Translate posI 0 $ Scale sizePecaToScale sizePecaToScale $ desenhaPeca e h

  Quando uma linha é desenha a proxima será desenhada pelo mesmo processo mas será feita uma translação para baixo sendo que o tamanho somado é o tamanho de uma peça em pixeis.

> pic : desenhaMapa t posx (posy+pixelPecaSize) bestSizeToScale pixelPecaSize e
-}

desenhaMapa :: Mapa -> Float -> Float -> Float -> Float -> EstadoGloss -> [Picture]
desenhaMapa [] _ _ _ _ _= []
desenhaMapa (h:t) posx posy bestSizeToScale pixelPecaSize e = pic : desenhaMapa t posx (posy+pixelPecaSize) bestSizeToScale pixelPecaSize e
  where
    pic = Translate posx (-posy) $ Pictures $ desenhaLinha e 0 pixelPecaSize bestSizeToScale h

{- | Função que desenha uma linha do mapa.

  Esta função usa a função 'desenhaPeca' para obter a imagem da peça e redimensiona através do melhor tamanho para a peça.
  
> pic = Translate posI 0 $ Scale sizePecaToScale sizePecaToScale $ desenhaPeca e h

  Esta peça nunca tem uma translação no eixo y ficando assim sempre na mesma linha. O que acontece nesta função é semelhante ao que acontece na 'desenhaMapa' e a peça sofre uma translação no eixo x 
somando o valor do tamanho de uma peça em pixeis para que fiquem todas seguidas na mesma linha.

> pic : desenhaLinha e (posI+sizePixelPeca) sizePixelPeca sizePecaToScale t
-}

desenhaLinha :: EstadoGloss -> Float -> Float -> Float -> [Peca] -> [Picture]
desenhaLinha e _ _ _ [] = []
desenhaLinha e posI sizePixelPeca sizePecaToScale (h:t) = pic : desenhaLinha e (posI+sizePixelPeca) sizePixelPeca sizePecaToScale t
  where
    pic = Translate posI 0 $ Scale sizePecaToScale sizePecaToScale $ desenhaPeca e h

-- | Função que vai buscar a lista de 'imagensJogo' às 'imagens' do 'EstadoGloss' e recebe uma peça, sendo que, mediante esta retorna a imagem correspondete na lista
desenhaPeca :: EstadoGloss -> Peca -> Picture
desenhaPeca e (Bloco Indestrutivel) = head . imagensJogo . imagens $ e
desenhaPeca e (Bloco Destrutivel)   = (imagensJogo . imagens $ e) !! 1
desenhaPeca e Vazia                 = (imagensJogo . imagens $ e) !! 2

{- | Função que recebe um 'EstadoGloss' e um 'Mapa' e devolve um tuplo com o melhor tamanho para redimensionar a peça e o seu tamanho em pixeis.

  Um dos problemas ao desenhar o mapa foi o facto de que muitas vezes este saía do seu espaço no ecrã. Deste modo, estabelece-se uma relação entre o largura e a altura do ecrã e do mapa e verifica-se aquela que melhor
se adequa de forma a caber o no espaço estabelecido. Assim, este valor para utilizar a função 'Scale' acaba por ser o que faz com que o mapa não exceda certos limites e vá ficando cada vez mais pequeno à medida que 
vão sendo criadas novas linhas.

@
 sizePecaX     = fromIntegral a *0.5 / fromIntegral (tlinha * 100)
 sizePecaY     = fromIntegral b *0.8 / fromIntegral (tcol * 100)
 bestSize      = if sizePecaY > sizePecaX then sizePecaX else sizePecaY
 pixelPecaSize = bestSize * 100
@

-}

pecaSize :: EstadoGloss -> Mapa -> (Float,Float)
pecaSize e m = (bestSize, pixelPecaSize)
  where
    (a,b)         = windowSize e
    (tlinha,tcol) = (length . head $ m, length m)
    sizePecaX     = fromIntegral a *0.5 / fromIntegral (tlinha * 100)
    sizePecaY     = fromIntegral b *0.8 / fromIntegral (tcol * 100)
    bestSize      = if sizePecaY > sizePecaX then sizePecaX else sizePecaY
    pixelPecaSize = bestSize * 100

-- | Função que obtém um tuplo com a posicao x e y da peça que também vai ser usada por outras funções tais como a 'desenhaTanques' ou 'resizeTiro'.
getPecaPos :: EstadoGloss -> Mapa -> (Float,Float)
getPecaPos e m = (-posX,-posY)
  where
    pixelPecaSize = snd $ pecaSize e m
    (tlinha,tcol) = (length . head $ m, length m)
    (posX,posY)   = ((pixelPecaSize * fromIntegral tlinha) / 2 - (pixelPecaSize / 2), (pixelPecaSize * fromIntegral tcol / 2) - (pixelPecaSize / 2))


-- * Funções relacionadas com a posição dos tanques.
{- | Função que devolve a lista dos jogadores com as posições mais perto dos cantos do mapa possível.

  Um dos problemas enfrentado com a possibilidade de se fornecer ao utilizar a opção de criar um mapa foi a probabilidade deste poder desenhar um bloco destrutível ou indestrutível num dos cantos do mapa
e por isso torna essa posição impossível de ser ocupada por um tanque.

  A posição do tanque será calculada através da função 'bestPosTank' passando como argumente o 'EstadoGloss' e as posições que se econtram nas extremidades do mapa para esta devolver
a posição vazia onde o tanque pode de facto ser colocado. 

@
 jog1 = Jogador (bestPosTank e (0,0))   B 5 5 5
 jog2 = Jogador (bestPosTank e (li,co)) C 5 5 5
 jog3 = Jogador (bestPosTank e (0,co))  E 5 5 5
 jog4 = Jogador (bestPosTank e (li,0))  D 5 5 5
@

-}
posTank :: EstadoGloss -> [Jogador]
posTank e = listjog
  where
    mapa    = mapaEstado . estado $ e
    (li,co) = (length mapa, length . head $ mapa)
    jog1    = Jogador (bestPosTank e (0,0))   B 5 5 5
    jog2    = Jogador (bestPosTank e (li,co)) C 5 5 5
    jog3    = Jogador (bestPosTank e (0,co))  E 5 5 5
    jog4    = Jogador (bestPosTank e (li,0))  D 5 5 5
    listjog = [jog1,jog2,jog3,jog4]

{- | Função que calcula a posicao vazia onde cabe um tanque mais próxima da posição fornecida. 

  Esta função relaciona 2 funções:

- 'checkPosVazia' que devolve todas as posições vazias onde cabem tanques.

- 'calculaPosMaisProx' que calcula a posição mais próxima numa lista de posições. 
-}

bestPosTank :: EstadoGloss -> Posicao -> Posicao
bestPosTank e (x,y) = calculaPosMaisProx (x,y) listPosVazias
  where
    m             = mapaEstado . estado $ e
    listPosVazias = checkPosVazia m

{- | Função que recebe um mapa e devolve todas as posicões vazias onde cabem tanques

=== Exemplo de utilização:
>>> checkPosVazia $ mapaInicial (4,4)
    [(1,1)]

=== Exemplo de utilização:
>>> checkPosVazia $ mapaInicial (8,8)
    [(1,1),(1,2),(1,3),(1,4),(1,5),(2,1),(2,2),(2,3),(2,4),(2,5),(3,1),(3,2),(3,3),(3,4),(3,5),(4,1),(4,2),(4,3),(4,4),(4,5),(5,1),(5,2),(5,3),(5,4),(5,5)]
-}

checkPosVazia :: Mapa -> [Posicao]
checkPosVazia m = posOndeCabemTanques $ posVazia m allPosGroup
  where
    allPos      = [(l,c) | l <- [0 .. pred . length $ m], c <- [0 .. pred . length . head $ m]]
    allPosGroup = groupN allPos $ length . head $ m
{- | Função que dada uma lista de posições retira aquelas onde o tanque não cabe com as suas 4 posições.

=== Exemplo de utilização:
>>> posOndeCabemTanques [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
    [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
-}
posOndeCabemTanques :: [Posicao] -> [Posicao]
posOndeCabemTanques [] = []
posOndeCabemTanques ((a,b):t) = if (a+1,b) `elem` ((a,b):t) && (a+1,b+1) `elem` ((a,b):t) && (a,b+1) `elem` ((a,b):t) 
                                   then (a,b): posOndeCabemTanques t
                                   else posOndeCabemTanques t

{- | Função que dado um mapa e as posicões todas do mapa devolve a lista de todas as posições vazias.

=== Exemplo de utilização:
>>> posVazia (mapaInicial (6,6)) [[(0,0),(0,1),(0,2),(0,3),(0,4),(0,5)],[(1,0),(1,1),(1,2),(1,3),(1,4),(1,5)],[(2,0),(2,1),(2,2),(2,3),(2,4),(2,5)],[(3,0),(3,1),(3,2),(3,3),(3,4),(3,5)],[(4,0),(4,1),(4,2),(4,3),(4,4),(4,5)],[(5,0),(5,1),(5,2),(5,3),(5,4),(5,5)]]
   [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
-}

posVazia :: Mapa -> [[Posicao]] -> [Posicao]
posVazia [] [] = []
posVazia (x:xs) (y:ys) = aux x y ++ posVazia xs ys
  where
    aux :: [Peca] -> [Posicao] -> [Posicao]
    aux [] [] = []
    aux (x:xs) (y:ys) = if x == Vazia then y : aux xs ys else aux xs ys
{- | Função que dada uma posição e uma lista de posições devolve a mais próxima que está na lista.

=== Exemplo de utilização:
>>> calculaPosMaisProx (7,7) [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
   (4,4)
-}
calculaPosMaisProx :: Posicao -> [Posicao] -> Posicao
calculaPosMaisProx pos listPos = if null listPos then (0,0) else listPos !! distMenor (map (aux pos) listPos)
  where
    aux :: Posicao -> Posicao -> Float
    aux (x,y) h = dist (x,y) h
{- | Função que devolve o índice do valor mais baixo de uma lista de distâncias.

=== Exemplo de utilização: 
>>> distMenor [2.77,5.45,1.36]
    2 

=== Exemplo de utilização: 
>>> distMenor [1.89,5.45,1.36,1.00]
    3
-}
distMenor :: [Float] -> Int
distMenor [] = -1
distMenor (h:t) = aux h 0 1 t
  where
    aux vm indmin indreal [] = indmin
    aux vm indmin indreal (x:xs) | x < vm    = aux x indreal (succ indreal) xs
                                 | otherwise = aux vm indmin (succ indreal) xs

{- | Função que calcula a distância entre dois pontos.

=== Exemplo de utilização: 
>>> dist (4,5) (2,2)
    3.6055512
-}                                 
dist :: Posicao -> Posicao -> Float
dist (a,b) (x,y) = sqrt $ fromIntegral (y-b)^2 + fromIntegral (x-a)^2                      

-- * Funções para desenhar tanques.

{- | Função que devolve uma lista de 'Picture' contendo as imagens dos tanques nas posições corretas.

  Esta função utiliza uma auxiliar que utiliza a lista de jogadores e a as imagens que se obtém no 'pictureTank' e através do indice do jogador utiliza a imagem do tanque correspondente.

> aux e listJogadores listPictures

  Existe também a verificação se o jogador tem ainda vidas e se sim então desenha o jogador senão continua a chamar a função ignorando esse jogador. Assim resolve-se o problema dos tanques que já foram destruídos de continuarem a ser desenhados.

@
 if vidasJogador jog > 0
 then desenhaTank e jog pic : aux e jogs pics
 else aux e jogs pics
@
-}
desenhaTanques :: EstadoGloss -> [Picture]
desenhaTanques e = aux e listJogadores listPictures
  where
    (listPictures, listJogadores) = (pictureTank e, jogadoresEstado . estado $ e)
    aux :: EstadoGloss -> [Jogador] -> [Picture] -> [Picture]
    aux _ [] _ = []
    aux _ _ [] = []
    aux e (jog:jogs) (pic:pics) = if vidasJogador jog > 0
                                     then desenhaTank e jog pic : aux e jogs pics
                                     else aux e jogs pics

{- | Função complementar da 'desenhaTanques' que trata de desenhar __apenas um tanque__ e colocá-lo na posição correta.

  Como a imagem do tanque é 2 vezes o tamanho de uma peça então a variável do tamanho para dar 'Scale' de uma peça que se obtém da função 'pecaSize' terá de ser o dobro para o tanque.

@
  (sizeToScale,realPixelSize) = pecaSize e mapa
  (posX,posY)                 = getPecaPos e mapa
  resizeTank                  = scale (sizeToScale*2) (sizeToScale*2) pic  
@

  Após isto é feita uma rotação segundo a direção do tanque usando a função 'direcaoToAngle' 

> rotateTank = Rotate (direcaoToAngle dir) resizeTank

  Posteriormente o tanque sofre uma translação que utiliza a posicao da peça e a posição do tanque mutliplicando pelo tamanho da peça ficando no lugar correto. 
@
 (a,b) = (posX + (realPixelSize/2) +realPixelSize*fromIntegral y, posY + (realPixelSize / 2) + realPixelSize * fromIntegral x)
 translateTank = Translate a (-b) rotateTank
@
-}

desenhaTank :: EstadoGloss -> Jogador -> Picture -> Picture
desenhaTank e (Jogador (x,y) dir v l c) pic = translateTank
  where
    mapa                        = mapaEstado . estado $ e
    (sizeToScale,realPixelSize) = pecaSize e mapa
    (posX,posY)                 = getPecaPos e mapa
    resizeTank                  = scale (sizeToScale*2) (sizeToScale*2) pic
    rotateTank                  = Rotate (direcaoToAngle dir) resizeTank
    (a,b)                       = (posX + (realPixelSize/2) +realPixelSize*fromIntegral y, posY + (realPixelSize / 2) + realPixelSize * fromIntegral x)
    translateTank               = Translate a (-b) rotateTank

{- | Função que calcula o ângulo através da direção.

=== Exemplo de utilização: 
>>> direcaoToAngle B
    180.0
-}
direcaoToAngle :: Direcao -> Float
direcaoToAngle = realToFrac . (*) 90 . fromEnum

-- | Função que vai buscar as 4 imagens dos tanques às 'imagensJogo' do parâmetro 'imagens' do 'EstadoGloss'
pictureTank :: EstadoGloss -> [Picture]
pictureTank e = take njog listTanks
  where
    njog = nJog e
    listTanks =take 4 $ drop 3 (imagensJogo.imagens $ e)

-- * Funções para desenhar tiros.
{- | Função principal do desenhaTiro que filtra a lista dos disparos pelos diferentes tiros para que a lista de 'Picture' final tenha os lasers como últimos elementos da lista e assim usando a função 'Pictures' estes possam se sobrepor aos outros tiros quando usados no jogo.

@
 listDisp = disparosEstado . estado $ e
 lasers   = filter (\d -> case d of {DisparoLaser{} -> True; _ -> False}) listDisp
 other    = filter (\d -> case d of {DisparoLaser{} -> False; _ -> True}) listDisp
 newDispList = other ++ lasers 
@

  A função auxiliar recebe um disparos e o 'EstadoGloss' e se for um laser então chama a função 'drawLaserPath', se não for chama a função 'resizeTiro'

@
 aux :: EstadoGloss -> Disparo -> Picture
 aux e d@(DisparoLaser n pos dir) = drawLaserPath e d
 aux e p = resizeTiro e p (pictureTiro e p)

@
-}
desenhaTiro :: EstadoGloss -> [Picture]
desenhaTiro e = map (aux e) newDispList
  where
    listDisp = disparosEstado . estado $ e
    lasers   = filter (\d -> case d of {DisparoLaser{} -> True; _ -> False}) listDisp
    other    = filter (\d -> case d of {DisparoLaser{} -> False; _ -> True}) listDisp
    newDispList = other ++ lasers 
    aux :: EstadoGloss -> Disparo -> Picture
    aux e d@(DisparoLaser n pos dir) = drawLaserPath e d
    aux e p = resizeTiro e p (pictureTiro e p)

{- | Função que permite desenhar o laser no Jogo.

  A função calcula todas as posições indestrutiveis do mapa: 

@
 mapa        = mapaEstado . estado $ e
 allPos      = [(l,c) | l <- [0 .. pred . length $ mapa], c <- [0 .. pred . length . head $ mapa]]
 allPosGroup = groupN allPos $ length . head $ mapa
 allI        = posIndestrutivel mapa allPosGroup
@

  Posteriormente faz o caminho todo do laser com recurso às funções 'allPosPath', sendo que se usa a função 'shotAsTwoBlocks' para fazer o laser contar com 2 blocos em vez de 1, testando se atinge um bloco indestrutivel, guardando todas as posições.

@
 laser          = tail $ laserPath (pos,dir) allI 
 tuploAsTwoPos  = map (uncurry shotAsTwoBlocks) [(pos,dir)]
 (pathNC, path) = (map (flip allPosPath allI) tuploAsTwoPos, nub . concat $ pathNC)
@

  A lista de posições é obtida através do uso da função 'take' de metade 'length' do path, pois o path são as posições do laser como 2 blocos e apenas pretende-se ficar com as 'PosicaoGrelha' da lista. Assim resolve-se o problema de ter o laser em várias posições e faz o teste se embate ou não nu bloco indestrutível de forma correta.

> laserCaminho = take (div (length path) 2) path

  A função auxiliar recebe a lista de lasers e envia para a função 'resizeTiro' em cada posição.

@
 aux :: EstadoGloss -> Disparo -> [Posicao] -> [Picture]
 aux e d [] = []
 aux e d@(DisparoLaser n pos dir) (h:t) = resizeTiro e d (pictureTiro e d) : aux e (DisparoLaser n h dir) t
@

-}

drawLaserPath :: EstadoGloss -> Disparo -> Picture
drawLaserPath e d@(DisparoLaser n pos dir) = Pictures $ aux e d laserCaminho
  where 
    mapa              = mapaEstado . estado $ e
    joglistPos        = map posicaoJogador (jogadoresEstado .estado $ e)
    semJog            = deleteN n joglistPos 
    allPos            = [(l,c) | l <- [0 .. pred . length $ mapa], c <- [0 .. pred . length . head $ mapa]]
    allPosGroup       = groupN allPos $ length . head $ mapa
    allI              = posIndestrutivel mapa allPosGroup
    laser             = tail $ laserPath (pos,dir) allI 
    tuploAsTwoPos     = map (uncurry shotAsTwoBlocks) [(pos,dir)]
    (pathNC, path)    = (map (flip allPosPath allI) tuploAsTwoPos, nub . concat $ pathNC)
    laserCaminho      = take (div (length path) 2) path
    aux :: EstadoGloss -> Disparo -> [Posicao] -> [Picture]
    aux e d [] = []
    aux e d@(DisparoLaser n pos dir) (h:t) = resizeTiro e d (pictureTiro e d) : aux e (DisparoLaser n h dir) t
{- | Função que devolve a picture do tiro na posição correta e com o tamanho correto.

  Se o tiro for um Choque então, como a imagem é 6 vezes maior que uma peça só é preciso dar resize da imagem para o tamanho de uma peça no jogo e ele automaticamente ocupa uma área 6x6.

> resTiroChoque = scale sizeToScale sizeToScale pic

  Posteriormente o laser é colocado na posição do jogador da mesma forma que acontece no 'desenhaTank'.

@
 (i,j)           = (posX + (realPixelSize / 2) + realPixelSize*fromIntegral b, posY + (realPixelSize / 2) + realPixelSize * fromIntegral a)
 translateChoque = Translate i (-j) resTiroChoque
@

  A diferença para os outros tipos de tiro é que é necessário um resize diferente e fazer uma rotação de acordo com a sua direção

@
 resTiroCL = scale (sizeToScale/2) sizeToScale pic
 rotateCL  = rotate (direcaoToAngle dir) resTiroCL
    
@

-}
resizeTiro :: EstadoGloss -> Disparo -> Picture -> Picture
resizeTiro e (DisparoChoque jog tick) pic = translateChoque
  where
    mapa                        = mapaEstado . estado $ e
    (sizeToScale,realPixelSize) = pecaSize e mapa
    (posX,posY)                 = getPecaPos e mapa
    resTiroChoque               = scale sizeToScale sizeToScale pic
    (Jogador (a,b) dir v l c)   = (jogadoresEstado . estado $ e) !! jog
    (i,j)                       = (posX + (realPixelSize / 2) + realPixelSize*fromIntegral b, posY + (realPixelSize / 2) + realPixelSize * fromIntegral a)
    translateChoque             = Translate i (-j) resTiroChoque
resizeTiro e d pic = translCL
  where
    mapa                        = mapaEstado . estado $ e
    ((x,y), dir)                = (posicaoDisparo d,direcaoDisparo d)
    (sizeToScale,realPixelSize) = pecaSize e mapa
    (posX,posY)                 = getPecaPos e mapa
    resTiroCL                   = scale (sizeToScale/2) sizeToScale pic
    rotateCL                    = rotate (direcaoToAngle dir) resTiroCL
    (a,b)                       = (posX +(realPixelSize/2)+realPixelSize*fromIntegral y, posY + (realPixelSize/2) + realPixelSize*fromIntegral x)
    translCL                    = Translate a (-b) rotateCL
{- | Função que devolve a picture do tiro na lista de 'imagensJogo'.

  Como são 4 imagens para cada tiro, sendo elas, para os 4 tipos de tanque, e estão guardadas apartir do 7 índice da lista então ao índice do jogador soma-se 7 para ir buscar as imagens dos canhões, soma-se 11 para ir buscar os lasers e 15 para os choques.
-}
pictureTiro :: EstadoGloss -> Disparo -> Picture
pictureTiro e (DisparoCanhao jog _ _) = (imagensJogo . imagens $ e) !! (jog + 7)
pictureTiro e (DisparoLaser jog _ _)  = (imagensJogo . imagens $ e) !! (jog + 11)
pictureTiro e (DisparoChoque jog _)   = (imagensJogo . imagens $ e) !! (jog + 15)
-- * Funções para desenhar o contador de ticks.

{- | Função que desenha o contador de ticks no fundo da tela do jogo.

  Os ticks são guardados no parâmetro 'tickCounter' do 'EstadoGloss'. Usa-se a função 'show' para transformar esse __Int__ numa __String__ e envia-se para a função auxiliar a __String__ após se usar a função reverse, o valor 0 e o 'EstadoGloss'.

> Pictures $ aux (reverse listcharTick) 0 e

  Utiliza-se o reverse para se colocar o número do contador quando entra nas dezenas ou centas se vá alargando para a esquerda.

@
 aux :: String -> Float -> EstadoGloss -> [Picture]
 aux [] _ _ = []
 aux (h:t) n e = desenhaTick e n (pictureTick e h) : aux t (n+1.0) e
@
-}
drawTicksCounter :: EstadoGloss -> Picture
drawTicksCounter e = Pictures $ aux (reverse listcharTick) 0 e
  where
    listcharTick = show . tickCounter $ e
    aux :: String -> Float -> EstadoGloss -> [Picture]
    aux [] _ _ = []
    aux (h:t) n e = desenhaTick e n (pictureTick e h) : aux t (n+1.0) e


{- | Função que desenha um Tick.

  Esta função calcula um novo espaço para o contador retirando à altura da tela o tamanho do mapa e divindo esse espaço por 4 ficando o contador mesmo por baixo do mapa.

>  newEspacoForTick = (fromIntegral y - realPixelSize * (fromIntegral . length $ mapa))/4
   
  Posteriormente a variável para dar 'Scale' ao tick é alterada para ser em função do tamanho desse novo espaço para que o tick seja redimensionado ao alterar-se o tamanho da tela.

@
 scaleNewEspaco  = newEspacoForTick/fromIntegral y
 resTickPic      = scale scaleNewEspaco scaleNewEspaco pic
 pixelSizeOfTick = scaleNewEspaco * 600
@

  De facto, define-se uma variável que seja o tamanho do tick em pixeis e multiplica-se o valor n para que seja corretamente colocado quando o valor passa por exemplo de 9 para 10.

@
 sumTranslate  = n * pixelSizeOfTick
 translateTick = Translate (-sumTranslate + pixelSizeOfTick) ((-realPixelSize * (fromIntegral . length $ mapa))/2 - pixelSizeOfTick/1.5) resTickPic 
@

-}
desenhaTick :: EstadoGloss -> Float -> Picture -> Picture
desenhaTick e n pic = translateTick
  where
    mapa                        = mapaEstado . estado $ e
    (x,y)                       = windowSize e
    (sizeToScale,realPixelSize) = pecaSize e mapa
    newEspacoForTick            = (fromIntegral y - realPixelSize * (fromIntegral . length $ mapa))/4
    scaleNewEspaco              = newEspacoForTick/fromIntegral y
    resTickPic                  = scale scaleNewEspaco scaleNewEspaco pic
    pixelSizeOfTick             = scaleNewEspaco * 600
    sumTranslate                = n * pixelSizeOfTick
    translateTick               = Translate (-sumTranslate + pixelSizeOfTick) ((-realPixelSize * (fromIntegral . length $ mapa))/2 - pixelSizeOfTick/1.5) resTickPic 


{- | Função que devolve a picture corresponde ao tick na lista 'imagensTickCounter' do 'EstadoGloss'.
-}
pictureTick :: EstadoGloss -> Char -> Picture
pictureTick e c = let numero = digitToInt c
                  in (imagensTickCounter . imagens $ e) !! numero
-- * Funções para desenhar os powerups.
{- | Função que desenha os powerups no jogo.

  É criada uma variável que guarda a lista de 'powerups' do 'EstadoGloss' e envia cada powerup para a função auxiliar juntamente com o 'EstadoGloss' atual. Este é depois redimensionado pela função 'resizePowerUp' e a sua picture é obtida pela 'picturePowerUp'.
-}
desenhaPowerUps :: EstadoGloss -> [Picture]
desenhaPowerUps e = map (aux e) listpowerup
  where
    listpowerup = powerups e
    aux :: EstadoGloss -> Powerup -> Picture
    aux e p = resizePowerUp e p (picturePowerUp e p)
{- | Função que redimensiona o tamanho do powerup para ser menor que o tamanho de uma peça para ficar colocada numa 'PosicaoGrelha'.

> resPU = scale (sizeToScale/8) ( sizeToScale/8) pic

  Assim sofre uma translação igual à dos tanques ou dos tiros e é colocada no lugar correto.

> (i,j) = (posX + (realPixelSize / 2) +realPixelSize * fromIntegral b , posY + (realPixelSize / 2) + realPixelSize*fromIntegral a)
-}
resizePowerUp :: EstadoGloss -> Powerup -> Picture -> Picture
resizePowerUp e powerup pic = translatePU
  where
    mapa                        = mapaEstado . estado $ e
    (sizeToScale,realPixelSize) = pecaSize e mapa
    (posX,posY)                 = getPecaPos e mapa
    resPU                       = scale (sizeToScale/8) ( sizeToScale/8) pic
    (a,b)                       = posicaoPowerUp powerup
    (i,j)                       = (posX + (realPixelSize / 2) +realPixelSize * fromIntegral b , posY + (realPixelSize / 2) + realPixelSize*fromIntegral a)
    translatePU                 = Translate i (-j) resPU
{- | Função que vai buscar a picture correspondente ao powerup à lista de 'imagensPowerUps' do parâmetro 'imagens' do 'EstadoGloss'.
-}
picturePowerUp :: EstadoGloss -> Powerup -> Picture
picturePowerUp e (PowerupLasers _)    = head . imagensPowerUps . imagens $ e
picturePowerUp e (PowerupChoques _)   = (imagensPowerUps . imagens $ e) !! 1
picturePowerUp e (PowerupTeleport _)  = (imagensPowerUps . imagens $ e) !! 2
picturePowerUp e (PowerupVida _)      = (imagensPowerUps . imagens $ e) !! 3

-- * Funções para desenhar o estado dos jogadores.

{- | Função responsável pelo desenho dos estados dos jogadores.

  Esta obtém a lista de jogadores atráves do 'Estado' do 'EstadoGloss' e envia para uma auxiliar que desenha cada estado, começando no jogador 0.

@
 aux :: [Jogador] -> Int -> EstadoGloss -> [Picture]
 aux [] _ _ = []
 aux (h:t) a e = desenhaEstadoJog e a : aux t (a+1) e
@
-}
getEstados :: EstadoGloss -> Picture
getEstados e = Pictures $ aux listJog 0 e
  where 
    listJog = jogadoresEstado . estado $ e 
    aux :: [Jogador] -> Int -> EstadoGloss -> [Picture]
    aux [] _ _ = []
    aux (h:t) a e = desenhaEstadoJog e a : aux t (a+1) e 

{- | Função que desenha um estado individual, sendo que, dependo do indice do jogador desenha num dos cantos da tela do jogo.

Se for o jogador 0 será colocado no canto __superior direito__.

Se for o jogador 1 será colocado no canto __inferior direito__.

Se for o jogador 2 será colocado no canto __superior esquerdo__.

Se for o jogador 3 será colocado no canto __inferior esquerdo__.

  Esta função calcula o espaço ao lado do mapa que fica reservado para os estados. Assim estes são colocados nesse espaço e ficam dependetes do tamanho desse espaço. Desta forma, se a tela for maximizada eles ficam corretamente colocados nos cantos da tela e aumentam de tamanho.

@
 newEspacoForEstado = (fromIntegral x-pixelPS * (fromIntegral . length . head $ mapa))/2
 newSizeToResize    = newEspacoForEstado / 900
@

Assim, sofrem uma translaçao em que ficam encostados às partes laterais da tela mas ficam com um espaço em relação ao fundo e ao topo da tela.

> (a,b) = (fromIntegral x /2 - sizeOfPixelEstado / 2,fromIntegral y / 2 - sizeOfPixelEstado/1.5)
-}

desenhaEstadoJog :: EstadoGloss -> Int -> Picture
desenhaEstadoJog e n = case n of 
                         0 -> desenhaInfo 0 a b sizeOfPixelEstado e $ Translate a b resizeEstado
                         1 -> desenhaInfo 1 a (-b)  sizeOfPixelEstado e $ Translate a (-b) resizeEstado
                         2 -> desenhaInfo 2 (-a) b sizeOfPixelEstado e $ Translate (-a) b resizeEstado
                         3 -> desenhaInfo 3 (-a) (-b) sizeOfPixelEstado e $ Translate (-a) (-b) resizeEstado
  where
    mapa               = mapaEstado . estado $ e
    pic                = estadoJog n e  
    (x,y)              = windowSize e
    (sizePeca,pixelPS) = pecaSize e mapa
    newEspacoForEstado = (fromIntegral x-pixelPS * (fromIntegral . length . head $ mapa))/2
    newSizeToResize    = newEspacoForEstado / 900
    sizeOfPixelEstado  = newSizeToResize * 600      
    resizeEstado       = scale newSizeToResize newSizeToResize pic
    (a,b)              = (fromIntegral x /2 - sizeOfPixelEstado / 2,fromIntegral y / 2 - sizeOfPixelEstado/1.5)

{- | Função que vai buscar às 'imagensJogo' a imagem do estado do tanque correspondente.
-}
estadoJog :: Int -> EstadoGloss -> Picture
estadoJog n e = pics !! (n+19)
  where pics = imagensJogo . imagens $ e

{- | Função que desenha o número de vidas, lasers e choques e os coloca no lugar certo do estado.

  Neste caso é usada uma estratégia semelhante ao 'drawTicksCounter' em que se cria uma string para as vidas, lasers e choque do jogador usando a função 'resizeVLC' para desenhar as pictures correpondentes aos números, redimensionando-os e posteriormente coloca-os no lugar correto.

@
 jog             = flip (!!) n . jogadoresEstado . estado $ e
 vidas           = show . vidasJogador $ jog
 laser           = show . lasersJogador $ jog
 choque          = show . choquesJogador $ jog
 infoAfterResize = resizeVLC vidas laser choque e sizeOfPixelEstado a b
@
-}
desenhaInfo :: Int -> Float -> Float -> Float -> EstadoGloss-> Picture -> Picture
desenhaInfo n a b sizeOfPixelEstado e pic = Pictures [pic,infoAfterResize]
  where 
    jog    = flip (!!) n . jogadoresEstado . estado $ e
    vidas  = show . vidasJogador $ jog
    laser  = show . lasersJogador $ jog
    choque = show . choquesJogador $ jog
    infoAfterResize = resizeVLC vidas laser choque e sizeOfPixelEstado a b

{- | Esta função redimensiona as imagens das informações para que estas sejam colocadas no lugar correto do estado.

  Assim é necessário que o tamanho a redimensionar seja em função do tamanho do estado do jogador e é preciso saber qual a translação do estado correspondente. Por outras palavras, é preciso saber os valor usados
na translação dos estados na função 'desenhaEstadoJog' para que as informações sejam colocadas no estado correto.

@
 translateNumber  = 400 * spe / 600
 translateNumberY = 160 * spe / 400

 picVida    = Pictures . reverse $ placeN e vidas 0 pixelNumberSize scaleVidas x y (-2.0)
 picLaser   = Pictures . reverse $ placeN e laser 0 pixelNumberSize scaleLaser x y (-1.0)
 picChoques = Pictures . reverse $ placeN e choque 0 pixelNumberSize scaleChoque x y 1.5
@

Obs: x e y são os valores da translação do estado.
-}
resizeVLC :: String -> String -> String -> EstadoGloss -> Float -> Float -> Float -> Picture
resizeVLC vidas laser choque e spe x y = Pictures list
  where
    translateNumber  = 400 * spe / 600
    translateNumberY = 160 * spe / 400
    fator            = translateNumber / 2500
    pixelNumberSize  = fator * 600.0
    (a,b)            = windowSize e 
    scaleVidas       = fator / fromIntegral (length vidas)
    scaleLaser       = fator / fromIntegral (length laser)
    scaleChoque      = fator / fromIntegral (length choque)  
    picVida          = Pictures . reverse $ placeN e vidas 0 pixelNumberSize scaleVidas x y (-2.0)
    picLaser         = Pictures . reverse $ placeN e laser 0 pixelNumberSize scaleLaser x y (-1.0)
    picChoques       = Pictures . reverse $ placeN e choque 0 pixelNumberSize scaleChoque x y 1.5
    list             = [picVida, picLaser,picChoques]
{- | Função que coloca uma informação no lugar certo dentro do estado.

  Esta recebe um multiplicador de tipo, ou seja, o valor que é necessário para colocar a informção ao lado da imagem que aparece dentro de estado relativa a essa informação.

> (a,b) = (x - s + fromIntegral n / 2 * s, y + s * multplicadorDeTipo)

  Esta função também utiliza a função 'pictureTick' para obter o número correspondente ao número de vidas, lasers e choques.
-}
placeN :: EstadoGloss -> String -> Int -> Float -> Float -> Float -> Float -> Float -> [Picture]
placeN _ [] _ _ _ _ _ _ = []
placeN e (h:t) n s scale x y multplicadorDeTipo = Translate a b (Scale scale scale $ pictureTick e h) : placeN e t (n+1) s scale x y multplicadorDeTipo
  where
    (a,b) = (x - s + fromIntegral n / 2 * s, y + s * multplicadorDeTipo)