-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g027 where
import Data.List
import LI11819

-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.
v1,v2 :: Vetor
v1 = (4,6)
v2 = (6,3)
-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor 
somaVetores (l1,c1) (l2,c2)= (l1+l2,c1+c2) -- Soma dois vetores

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (l1,c1) (l2,c2)= (l1-l2,c1-c2) -- Subtrai dois vetores

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor a (l1,c1) = (a*l1,c1*a) 

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distânciaa à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (l1,c1) = (c1,-l1)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (l1,c1) = (-l1,c1)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (l1,c1) = (l1,-c1)

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor a | a == C = (-1,0)
                   | a == D = (0,1)
                   | a == B = (1,0)
                   | a == E = (0,-1)
                   
-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido _ [] = False
eIndiceListaValido a l = a <= (length l-1) && a >= 0 

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz l = if length (head l) == 0 then (0,0) else (length l, length (head (l)))  

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (a,b) l = ((a < length l) && (a >= 0)) && ((b < length (head l)) && (b >= 0))

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (l,c) y = (l == 0 && (c <= length (head y)-1)) || (l == (length y-1)  && (c<= (length (head y)-1))) || ((l > 0 && l < length y-1) && (c==0|| c== length (head y)-1))

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--

-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz I =  [[False,True,False,False],
                          [False,True,False,False],
                          [False,True,False,False],
                          [False,True,False,False]]

tetrominoParaMatriz J =[[False,True,False],
                        [False,True,False],
                        [True,True,False]]

tetrominoParaMatriz L = [[False,True,False],
                         [False,True,False],
                         [False,True,True]]
                         
tetrominoParaMatriz Z =[[True,True,False],
                        [False,True,True],
                        [False,False,False]]


tetrominoParaMatriz S=[[False,True,True],
                       [True,True,False],
                       [False,False,False]]

tetrominoParaMatriz T =[[False,False,False],
                        [True,True,True],
                        [False,True,False]]


tetrominoParaMatriz O =[[True,True],
                        [True,True]]

 
-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista a l = l !! a

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista a elemento li = fst (splitAt a li) ++ [elemento] ++ (tail (snd(splitAt a li)))
-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a ->Matriz a
rodaMatriz [] = []
rodaMatriz ma = transpose (reverse ma)


-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH a = map reverse a

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV a = reverse a

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (a,b) c = replicate a (replicate b c)

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (a,b) c = c !! a !! b

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (a,b) c ma = if ePosicaoMatrizValida (a,b) ma then fst (splitAt a ma) ++ [take b (ma !! a) ++ [c]++ drop (b+1) (ma !! a)] ++ tail (snd (splitAt a ma)) else ma



