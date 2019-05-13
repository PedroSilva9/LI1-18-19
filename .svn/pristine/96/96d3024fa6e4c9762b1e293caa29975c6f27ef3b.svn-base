{-|
Module      : Tarefa1_2018li1g027
Description : Tarefa 1 do projeto a desenvolver no âmbito da disciplina de LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>
Tarefa 1 do projeto a desenvolver no âmbito da unidade curricular LI1.
O objetivo desta tarefa é criar um mapa através de um conjunto de instruções.
-}

module Tarefa1_2018li1g027 where

import LI11819
import Data.List (transpose)
import Matrizes (Matriz,allM)
import TestesT1 (testes)

-- | Um vetor que representa a direção com coordenadas.
type Vetor = (Int,Int)

-- * Conjunto de testes
-- | Testes da tarefa 1. /Para ver exemplos de testes aceder ao ficheiro 'TestesT1'. /
testesT1 :: [Instrucoes]
testesT1 = testes

{- | Função que a partir de uma instrução calcula o novo estado do editor.

== Exemplo de utilização:
>>> instrucao (Move D) (editorInicial . head $ testesT1)
   Editor {posicaoEditor = (4,5), direcaoEditor = C, tetrominoEditor = I, paredeEditor = Indestrutivel, mapaEditor = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]}
-}

instrucao :: Instrucao -> Editor -> Editor
instrucao (Move d) (Editor pos dir tet wall mapa) = Editor newP dir tet wall mapa
  where 
    (a,b) = direcaoParaVetor d
    (x,y) = pos
    newP = (a + x, b + y)
instrucao Roda (Editor pos dir tet wall mapa) = Editor pos newD tet wall mapa
  where 
    newD = toEnum . (flip mod 4) . succ . fromEnum $ dir
instrucao MudaTetromino (Editor pos dir tet wall mapa) = Editor pos dir newT wall mapa 
  where
    newT = toEnum . (flip mod 7) . succ . fromEnum $ tet
instrucao MudaParede (Editor pos dir tet wall mapa) = Editor pos dir tet newWall mapa
  where
    newWall = toEnum . (flip mod 2) . succ . fromEnum $ wall
instrucao Desenha (Editor pos dir tet wall mapa) = Editor pos dir tet wall newMapa
  where
    tetpos = targets pos $ rotateMatrix dir $ (!!) allM $ fromEnum tet
    newMapa = desenha mapa wall tetpos

-- ** Execução de um conjunto de instruções num editor de mapas.
{- | Faz a execução de um conjunto de instruções, resultando num novo editor de mapas. Esta função utiliza a função instrucao.

== Exemplo de Utilização:
>>> instrucoes (head testesT1) (editorInicial . head $ testesT1)
Editor {posicaoEditor = (4,9), direcaoEditor = D, tetrominoEditor = L, paredeEditor = Indestrutivel, mapaEditor = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]}
-}

instrucoes :: Instrucoes -> Editor -> Editor
instrucoes t e = foldl (flip instrucao) e t

-- ** Construção de um mapa inicial

{- | Recebe uma dimensão e cria um mapa inicial com paredes nas bordas e o resto vazio.
 
== Exemplo de Utilização:
>>> mapaInicial (6,6)
[[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
-}

mapaInicial :: Dimensao -> Mapa
mapaInicial (l,c) = let mapa = map (\p -> (Bloco Indestrutivel) : p ++ [Bloco Indestrutivel]) $ mapaVazio (l-2, c-2)
                    in parede : mapa ++ [parede] where
    parede = replicate c $ Bloco Indestrutivel
    mapaVazio :: Dimensao -> Mapa
    mapaVazio (li,co) = replicate li $ replicate co Vazia

-- ** Rotação de uma matriz

{- | Função que roda uma matriz consoante a direção atual do editor. 
 
== Exemplo de Utilização:
>>> rotateMatrix D (allM !! 1)
[[True,False,False],[True,True,True],[False,False,False]]
-}

rotateMatrix :: Direcao -> Matriz Bool -> Matriz Bool
rotateMatrix C m = m
rotateMatrix D m = transpose . reverse $ m
rotateMatrix B m = reverse . transpose . reverse . transpose $ m
rotateMatrix E m = transpose . map reverse $ m

-- ** Passagem de uma direção para um vetor

{- | Função que transforma uma direção num par ordenado, correspondendo ao vetor direção. 
 
== Exemplo de Utilização:
>>> direcaoParaVetor C
(-1,0)

>>> direcaoParaVetor D
(0,1)
-}

direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor C = (-1,0)
direcaoParaVetor D = (0,1)
direcaoParaVetor B = (1,0)
direcaoParaVetor E = (0,-1)

-- ** Construção do editor inicial

{- | A construção de um editor relaciona, essencialmente, 3 funções:
   
   * 'mapaInicial'
   
   * 'dimensaoInicial'
   
   * 'posicaoInicial'
-}

{- | Cria um editor inicial, usando a peça I indestrutível voltada para cima.
 
== Exemplo de Utilização:
>>> editorInicial . head $ testesT1
Editor {posicaoEditor = (4,4), direcaoEditor = C, tetrominoEditor = I, paredeEditor = Indestrutivel, mapaEditor = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]}
-}

editorInicial :: Instrucoes -> Editor
editorInicial i =  Editor (posicaoInicial i) C I Indestrutivel (mapaInicial . dimensaoInicial $ i)

-- ** Cálculo das posições do mapa que serão mudadas.

{- | Função que recebendo a posição do editor e uma matriz booleana, retira as posições de todas os True's existentes na matriz booleana.
 
== Exemplo de Utilização:
>>> targets (1,1) (allM !! 1)
[(1,2),(2,2),(3,1),(3,2)]
-}

targets :: Posicao -> Matriz Bool -> [Posicao]
targets _ [] = []
targets (l,c) (h:t) = target (l,c) h ++ targets (l+1,c) t
  where
    target :: Posicao -> [Bool] -> [Posicao]
    target _ [] = []
    target (l,c) (h:t) | h = (l,c) : target (l,c+1) t 
                       | not h =  target (l,c+1) t

-- ** Atualiza uma posição numa lista.

{- | Função que recebe o índice da posição a mudar, assim como o novo valor a colocar nesse índice da lista.
 
== Exemplo de Utilização:
>>> atualizaIndiceLista 2 3 [1,1,1,1]
[1,1,3,1]
-}

atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = [] 
atualizaIndiceLista 0 e (_:t) = e : t
atualizaIndiceLista i e (h:t) = h : atualizaIndiceLista (i-1) e t

-- ** Atualiza uma posição numa matriz.

{- | Utilizando a função 'atualizaIndiceLista' , atualiza uma posição numa matriz.
 
== Exemplo de Utilização:
>>> atualizaPosicaoMatriz (1,1) 2 $ replicate 3 [1,1,1]
[[1,1,1],[1,2,1],[1,1,1]]
-}

atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (0, y) a (h:t) = atualizaIndiceLista y a h : t
atualizaPosicaoMatriz (x, y) a (h:t) = h : atualizaPosicaoMatriz (x-1, y) a t
atualizaPosicaoMatriz _ _ [] = []

-- ** Desenha num mapa um tetrómino.

{- | Utilizando a função 'atualizaIndiceMatiz' , desenha num mapa o tetrómino atual do editor, resultando num novo mapa.
 
== Exemplo de Utilização:
>>> desenha (mapaInicial (6,6)) Destrutivel [(2,2),(2,3)]
[[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
-}

desenha :: Mapa -> Parede -> [Posicao] -> Mapa
desenha m _ [] = m
desenha m wall (h:t) = desenha (atualizaPosicaoMatriz h (Bloco wall) m) wall t

-- ** Construção do Mapa

{- | A construção de um mapa relaciona, essencialmente, 3 funções:
   
   * 'mapaEditor'
   
   * 'instrucoes'
   
   * 'editorInicial'
-}

{- | Recebe um conjunto de instruções e devolve o mapa resultante.
 
== Exemplo de Utilização:
>>> constroi . head $ testesT1
[[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
-}
constroi :: Instrucoes -> Mapa
constroi i = mapaEditor . (instrucoes i) .  editorInicial $ i
