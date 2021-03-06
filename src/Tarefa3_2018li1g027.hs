{-|
Module      : Tarefa3_2018li1g027
Description : Tarefa 3 do projeto a desenvolver no âmbito da disciplina de LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>

= Introdução

Este módulo contém todas as funções relativas à compressão de um estado e descompressão de uma string.

= Objetivo

O objetivo desta tarefa é comprimir um estado para String e fazer o processo inverso obtendo o estado inicial. Assim a estratégia pensada é a de substituir padrões na string por caractéres, tornando a string
cada vez mais curta.

= Conclusão

Concluindo, esta tarefa permitiu utilzar estratégias de substituição em strings, obtendo mais conhecimentos relativos a alterações de strings utilizando funções como 
'ord' e 'chr' para que certas informações da string possam ser transformadas em apenas um caractér.
O resultado final foi positivo, sendo que, o objetivo inicial foi cumprido tendo obtido uma taxa de compressão aceitável.

-}

module Tarefa3_2018li1g027 where

import LI11819
import Data.Char (chr,ord)
import TestesT3 (test)

-- * Conjunto de testes
-- | Testes da tarefa 3. Cada teste é um 'Estado'. /Para ver exemplos de testes aceder ao ficheiro 'TestesT3'. /
testesT3 :: [Estado]
testesT3 = test 

{- | Função que transforma um caracter numa string.

== Exemplo de utilização:
>>> toString 'c'
    "c" 
-}
toString :: Char -> String
toString c = [c]

-- * Compressão de um mapa para uma String.
{- | Função que comprime um mapa para uma String.

== Exemplo de utilização:
>>> transformaMapa . mapaEstado .head $ testesT3
    "\n\n,\a?\bV,"
-}
transformaMapa :: Mapa -> String 
transformaMapa [] = []
transformaMapa l = chr mlinhas : chr mColunas : strMapaInterior
    where mColunas = length $ head l 
          mlinhas = length l
          strTotal = agrupaPI $ agrupaPecasVaziasPE (length(head l) -2) (vchaveta2substitute . identificaPecasUnitariasJuntas . mapaParaIdent $ l) 
          strSemPrimeiraLinha = drop 2 strTotal   
          strMapaInterior = take ((length strSemPrimeiraLinha)-2) strSemPrimeiraLinha 

{- |Função que transforma um mapa numa String, sendo que, cada peça é subsituida pelo seu identificador.

== Exemplo de utilização:
>>>  mapaParaIdent . mapaEstado . head $ testesT3
  "\nI,\bV,,\bV,,\bV,,\bV,,\bV,,\bV,,\bV,,\bV,\nI"
-}

mapaParaIdent :: Mapa -> String
mapaParaIdent [] = ""
mapaParaIdent l = (substituiPecaUnitaria . linhaMapa $ head l) ++ mapaParaIdent (tail l)

{- |Função que transforma uma linha do mapa numa String,sendo que, cada peça é passa a ser representada pelo seu identificador.

== Exemplo de utilização:
>>>  linhaMapa [Bloco Indestrutivel, Vazia, Vazia, Bloco Indestrutivel]
    "\SOHI\STXV\SOHI"
-}

linhaMapa :: [Peca] -> String
linhaMapa [] = []
linhaMapa peca = nTimes ++ linhaMapa resto
  where
    tuplo  = span (==head peca) peca
    nTimes = (toString . chr . length . fst $ tuplo) ++ (identificadorDePeca . head $ peca)
    resto  = snd tuplo


{- |Função que transforma um caracter por outro identificador no caso de essa peça ser no mapa uma peça unitária, ou seja, se for 1V, 1D ou 1I.

== Exemplo de utilização:
>>> identificadorUnitario 'I'
  ','      
-}


identificadorUnitario :: Char -> Char
identificadorUnitario c = if c == 'V' then '-' else if c == 'D' then '.' else ','


{- |Função que converte todas as peças unitárias de uma string por outro identificador.

== Exemplo de utilização:
>>> substituiPecaUnitaria "\SOHI\STXV\SOHI"
    ",\STXV,"    
-}

substituiPecaUnitaria :: String -> String
substituiPecaUnitaria [] = []
substituiPecaUnitaria l = let (t,h) = (take 1 l, drop 1 l)
                          in if (ord . head $ t) == 1 then [identificadorUnitario (head h)] ++ substituiPecaUnitaria (tail h) else t ++ [(head h)] ++ (substituiPecaUnitaria (tail h))  


{- |Função que converte todas as peças unitárias que estejam seguidas uma da outra na string pelo caracter '}'.

== Exemplo de utilização:
>>> identificaPecasUnitariasJuntas  "\STXV,,\SOHV"
    "\STXV}\SOHV"        
-}

identificaPecasUnitariasJuntas :: String -> String 
identificaPecasUnitariasJuntas [] = []
identificaPecasUnitariasJuntas l = let (l1,l2) = span (/=',') l
                                       resto = l2
                                   in if l2 == [] then l1 ++ (identificaPecasUnitariasJuntas l2) else if head l2 == head (tail resto) then l1 ++ "}" ++ (identificaPecasUnitariasJuntas (tail (tail resto))) else l1++ "," ++ identificaPecasUnitariasJuntas (tail resto)


{- |Função que converte identificadores de blocos vazios seguidos de identificadores de peças unitárias que estavam juntas.

== Exemplo de utilização:
>>> vchaveta2substitute "\STXV}\SOHV}"
    "\STX!\SOH!"      
-}

vchaveta2substitute :: String -> String 
vchaveta2substitute [] = []
vchaveta2substitute l = let (l1,l2) = span (/= 'V') l
                        in if l2 == [] then l1 else if (take 2 l2) == "V}" then l1 ++"!" ++ (vchaveta2substitute $ drop 2 l2) else l1 ++ "V" ++ (vchaveta2substitute . tail $ l2)

{- |Função que recebe o tamanho de uma linha da string e substitui na string sempre que aparece o chr (tamanho da linha do mapa - 2) seguido de um '!' por um '?'.

== Exemplo de utilização:
>>> agrupaPecasVaziasPE 1 "\STX!\SOH!\SOH!"
    "\STX!??"      
-}

agrupaPecasVaziasPE :: Int -> String -> String 
agrupaPecasVaziasPE _ [] = []
agrupaPecasVaziasPE (mCol) l = if l2 == [] then l1 else if a1 == mCol then  a2 ++ "?" ++ agrupaPecasVaziasPE mCol (tail l2) else l1 ++ "!" ++ agrupaPecasVaziasPE mCol (tail l2)       
    where (l1,l2) = span (/= '!') l
          a1 = (ord . head $ reverse l1)
          a2 = reverse . tail . reverse $ l1


{- |Função agrupa todos os '?' de uma string.

== Exemplo de utilização:
>>> agrupaPI "\STX!???\STX!"
    "\STX!\ETX?\STX!"   
-}

agrupaPI :: String -> String 
agrupaPI [] = []
agrupaPI l = let (l1,l2) = span (/= '?') l
             in if l2 == [] then l1 else l1 ++ (repetePecas $ takeWhile (== '?') l2) ++ (agrupaPI $ dropWhile (=='?') l2)


{- |Função usada para agrupar caracteres.

== Exemplo de utilização:
>>> repetePecas "VVV"
    "\ETXV"   
-}

repetePecas :: String -> String  
repetePecas [] = ""
repetePecas l = aux x ++ repetePecas y 
   where (x,y) = span (== head l) l
         aux :: String -> String
         aux [] = ""
         aux x = let (l1,l2) = span (== head x) x
                 in (chr .length $ l1) : [head x] ++ l2               


{- |Função que transforma uma peça num identificador.

== Exemplo de utilização:
>>>  identificadorDePeca (Bloco Indestrutivel)
    'I'  
-}

identificadorDePeca :: Peca -> String
identificadorDePeca (Bloco Indestrutivel) = "I" 
identificadorDePeca (Bloco Destrutivel) = "D"
identificadorDePeca (Vazia) = "V"

-- * Descompressão de uma String para um mapa.

{- |Função principal da descompressão do mapa, que converte uma string para um mapa.

== Exemplo de utilização:
>>>  str2mp "\n\n,\a?\bV,"
    [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
-}

str2mp :: String -> Mapa 
str2mp [] = []
str2mp l = (stringParaMapa.tail .pecasUni2Normal .pecasUniJuntas2PecasUni .pi2Vchaveta $ transformaPIemPE (linhaLength - 2) (reversePIrepetition str))
    where linhaLength = ord $ (!!) l 1 
          lista = drop 2 l 
          coordenadas = take 2 l 
          str = coordenadas ++ [(chr linhaLength)] ++"I"++ lista ++ [(chr linhaLength)] ++ "I" 

{- |Função que replica uma certa quantidade de '?'.

== Exemplo de utilização:
>>>  reversePIrepetition "\STXV\EOT?\STXV"
    "\STXV????\STXV"    
-}


reversePIrepetition :: String -> String 
reversePIrepetition [] = []
reversePIrepetition l = if l2 == [] then l1 else getstrAntesnTimesPI ++ (replicate nTimesPI '?') ++ reversePIrepetition (tail l2)
    where (l1,l2) = span (/= '?') l
          nTimesPI = ord . head $ take 1 (reverse l1) 
          lenStrAntesNTimes = length $ drop 1 (reverse l1)
          getstrAntesnTimesPI = take lenStrAntesNTimes l 
{- |Função que recebe o tamanho de uma linha subtraíndo 2 unidades e converte todos os '?' em '!' na String.

== Exemplo de utilização (mapa de tamanho 10 por 10 logo o valor do tamanho da linha - 2 é 8):
>>>  transformaPIemPE 8 "\n\n,???????\bV,"
    "\n\n,\b!\b!\b!\b!\b!\b!\b!\bV,"   
-}


transformaPIemPE :: Int -> String -> String 
transformaPIemPE _ [] = []
transformaPIemPE a l = if head l == '?' then (chr a) : "!" ++ transformaPIemPE a (tail l) else [head l] ++ transformaPIemPE a (tail l)  

{- |Função que converte todos os '!' em 'V}' de uma String.

== Exemplo de utilização:
>>>  pi2Vchaveta "\n\n,\b!\b!\b!\b!\b!\b!\b!\bV,"
    "\n\n,\bV}\bV}\bV}\bV}\bV}\bV}\bV}\bV,"   
-}

pi2Vchaveta :: String -> String -- Transforma '!' em "V}"
pi2Vchaveta [] = []
pi2Vchaveta l = if head l == '!' then "V}" ++ (pi2Vchaveta (tail l)) else [head l] ++ pi2Vchaveta (tail l) 


{- |Função que converte todo o caracter '}' em ",," numa string.

== Exemplo de utilização:
>>>  pi2Vchaveta "\n\n,\bV}\bV}\bV}\bV}\bV}\bV}\bV}\bV,"  
    "\n\n,\bV,,\bV,,\bV,,\bV,,\bV,,\bV,,\bV,,\bV,"
-}

pecasUniJuntas2PecasUni :: String -> String
pecasUniJuntas2PecasUni [] = []
pecasUniJuntas2PecasUni l = let (l1,l2) = span (/='}') l
                            in if l2 == [] then l1 else l1 ++ ",," ++ pecasUniJuntas2PecasUni (tail l2)  


{- |Função que converte todos o caracteres identificadores de peças unitárias em identificadores de peças normais, juntando o chr 1 antes do identificador.

== Exemplo de utilização:
>>>  pi2Vchaveta "\n\n,\bV,,\bV,,\bV,,\bV,,\bV,,\bV,,\bV,,\bV,"  
    "\n\n\SOHI\bV\SOHI\SOHI\bV\SOHI\SOHI\bV\SOHI\SOHI\bV\SOHI\SOHI\bV\SOHI\SOHI\bV\SOHI\SOHI\bV\SOHI\SOHI\bV\SOHI"
-}

pecasUni2Normal :: String -> String 
pecasUni2Normal [] = []
pecasUni2Normal l = let (l1,l2) = span (\c -> verificaIndicePU c) l 
                    in if l2 == [] then l1 else l1 ++ "\SOH" ++ subsPU2Peca (head l2) ++ pecasUni2Normal (tail l2)    


{- |Função que converte um identificador de peça unitária em identificador de peça normal.

== Exemplo de utilização:
>>>  subsPU2Peca '-'
    "V"
-}
subsPU2Peca :: Char -> String 
subsPU2Peca '.' = "D"
subsPU2Peca ',' = "I"
subsPU2Peca '-' = "V"

{- |Função verifica se o caracter não é um identificador de uma peca unitária.

== Exemplo de utilização:
>>>  subsPU2Peca 'V'
    True
-}

verificaIndicePU :: Char -> Bool 
verificaIndicePU x = x /= '.' && x /= ',' && x /= '-'

{- |Função que pega num conjunto de caracteres dois a dois até que a soma da quantidade de cada peça seja igual ao tamanho dado à função.

== Exemplo de utilização:
>>>  takeUntil 11 0 "\bI\ETXV\SOHI" 
    "\bI\ETXV"
-}

takeUntil :: Int -> Int -> String -> String
takeUntil _ _ [] = []
takeUntil len sum (h:x:t) | ord h + sum <= len = h : x : takeUntil len (sum + ord h) t
                  | otherwise = []

{- |Função que transforma uma String num mapa.

== Exemplo de utilização:
>>> takeUntil  4 0 "\EOT\SOHI\STXV\SOHI\SOHI\STXV\SOHI"
    "\EOT\SOH"

-}                  

stringParaMapa :: String -> Mapa
stringParaMapa [] = []
stringParaMapa str = stringParaMapaAux len strmapa
  where
    len = ord . head $ str
    strmapa = drop 1 str

{- |Função auxiliar da stringParaMapa que recebe o comprimento de uma linha do mapa e a string do mapa convertendo essa string para mapa linha a linha.

== Exemplo de utilização: 
>>> stringParaMapa "\EOT\SOHI\STXV\SOHI\SOHI\STXV\SOHI"
    [[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel]]


-}         

stringParaMapaAux :: Int -> String -> Mapa
stringParaMapaAux _ [] = []
stringParaMapaAux len str = stringLinhaMapa linha : stringParaMapaAux len resto
  where
    linha = takeUntil len 0 str
    resto = drop (length linha) str

{- |Função que transforma uma linha do mapa em String para uma linha do mapa.

== Exemplo de utilização:
>>> stringParaMapaAux 4 "\SOHI\STXV\SOHI\SOHI\STXV\SOHI"
    [[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel]]
-}         

stringLinhaMapa :: String -> [Peca]
stringLinhaMapa [] = []
stringLinhaMapa (h:x:t) = replicate nTimes peca ++ stringLinhaMapa t
  where
    nTimes = ord h
    peca = fromCharToPeca x
    
{- |Função que transforma um caracter identificador de uma peça na respetiva peça.

== Exemplo de utilização:
>>> stringLinhaMapa "\SOHI\STXV\SOHI"
    [Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel]
-}         

fromCharToPeca :: Char -> Peca 
fromCharToPeca 'I' = Bloco Indestrutivel
fromCharToPeca 'D' = Bloco Destrutivel
fromCharToPeca 'V' = Vazia

-- * Compressão dos jogadores para String.
{- |Função que converte uma lista de jogadores em String.

== Exemplo de utilização:
>>> jogadoresParaString [(Jogador (1,1) B 1 2 3),Jogador (2,2) C 2 3 4]
    "\SOH\SOHB\SOH\STX\ETX\STX\STXC\STX\ETX\EOT"
-}     

jogadoresParaString :: [Jogador] -> String
jogadoresParaString [] = []
jogadoresParaString l = jogadoresAsString  
  where
    jogadoresAsString = concat $ map jogadorParaString l

{- |Função que converte um jogador em String.

== Exemplo de utilização:
>>> jogadorParaString (Jogador (1,1) B 1 2 3)
    "\SOH\SOHB\SOH\STX\ETX"
-}     

jogadorParaString :: Jogador -> String
jogadorParaString (Jogador p d v l c) = x ++ y ++ (show d) ++ vida ++ laser ++ choque
  where
    x = toString . chr . fst $ p
    y = toString . chr . snd $ p
    (vida,laser,choque) = (toString . chr $ v, toString . chr $ l, toString . chr $ c)

-- * Descompressão de uma string para uma lista de jogadores.
{- |Função que converte uma string para uma lista de jogadores.

== Exemplo de utilização:
>>> stringParaJogadores "\SOH\SOHB\SOH\STX\ETX\STX\STXC\STX\ETX\EOT"
    [Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 2, choquesJogador = 3},Jogador {posicaoJogador = (2,2), direcaoJogador = C, vidasJogador = 2, lasersJogador = 3, choquesJogador = 4}]
-}     

stringParaJogadores :: String -> [Jogador]
stringParaJogadores [] = []
stringParaJogadores str = jogador : stringParaJogadores newStr
  where
    jogador = stringParaJogador $ take 6 str 
    newStr = drop 6 str

{- |Função que converte uma string para uma lista de jogadores.

== Exemplo de utilização:
>>> stringParaJogador "\SOH\SOHB\SOH\STX\ETX"
    Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 2, choquesJogador = 3}
-}         
stringParaJogador :: String -> Jogador
stringParaJogador str = Jogador pos dir v l c 
  where
    pos = (ord $ str !! 0, ord $ str !! 1)
    dir = read . toString $ str !! 2 :: Direcao
    (v,l,c) = (ord $ str !! 3, ord $ str !! 4, ord $ str !! 5)

-- * Compressão de uma lista de disparos para String.
{- |Função que transforma uma lista de disparos numa String.

== Exemplo de utilização:
>>> disparosParaString [(DisparoChoque 1 5),(DisparoCanhao 1 (1,2) B)]
    "C\SOH\ENQK\SOH\SOH\STXB"
-}        
disparosParaString :: [Disparo] -> String
disparosParaString [] = []
disparosParaString l = concat $ map disparoParaString l

{- |Função que converte um disparo para String.

== Exemplo de utilização:
>>> disparoParaString (DisparoChoque 1 5)
    "C\SOH\ENQ"    
-}     
disparoParaString :: Disparo -> String
disparoParaString (DisparoCanhao ind (x,y) dir) = "K" ++ [chr ind] ++ [chr x] ++ [chr y] ++ (show dir)
disparoParaString (DisparoLaser  ind (x,y) dir) = "L" ++ [chr ind] ++ [chr x] ++ [chr y] ++ (show dir)
disparoParaString (DisparoChoque ind tic)       = "C" ++ [chr ind] ++ [chr tic]

-- * Descompressão de uma String para uma lista de disparos.
{- |Função que transforma uma String numa lista de disparos.

== Exemplo de utilização:
>>> stringParaDisparos "C\SOH\ENQK\SOH\SOH\STXB
    [DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5},DisparoCanhao {jogadorDisparo = 1, posicaoDisparo = (1,2), direcaoDisparo = B}]   
-}     
stringParaDisparos :: String -> [Disparo]
stringParaDisparos [] = []
stringParaDisparos l | head l == 'K' = stringParaDisparo 'K' strLK : stringParaDisparos restoLK
                     | head l == 'L' = stringParaDisparo 'L' strLK : stringParaDisparos restoLK
                     | head l == 'C' = stringParaDisparo 'C' strC : stringParaDisparos restoC
  where
    strLK = take 4 . tail $ l
    restoLK = drop 5 l
    strC = take 2 . tail $ l
    restoC = drop 3 l

{- |Função que recebe o caracter que identifica o tipo de disparo e a string com a restante informação desse disparo convertendo-a num disparo.

== Exemplo de utilização:
>>> stringParaDisparo 'C' "\SOH\ENQ"
    DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5}  
-}     
stringParaDisparo :: Char -> String -> Disparo
stringParaDisparo disp str | disp == 'K' = DisparoCanhao ind pos dir
                           | disp == 'L' = DisparoLaser ind pos dir
                           | disp == 'C' = DisparoChoque ind tick
  where
    ind = ord $ str !! 0
    pos = (ord $ str !! 1, ord $ str !! 2)
    dir = read . toString $ str !! 3
    tick = ord $ str !! 1

-- * Compressão do estado.
{- |Função comprime um 'Estado' para formato textual.

== Exemplo de utilização:
>>> comprime . head $ TestesT3.test 
    "\n\n,\a?\bV,|\SOH\SOHB\SOH\SOH\SOH\ENQ\SOHD\SOH\SOH\SOH|C\SOH\ENQ"  
-}     
comprime :: Estado -> String
comprime e = mapa ++ "|" ++ jogadores ++ "|" ++ disparos
  where
    mapa = transformaMapa . mapaEstado $ e
    jogadores = jogadoresParaString . jogadoresEstado $ e
    disparos = disparosParaString . disparosEstado $ e

-- * Descompressão do estado.
{- |Função que faz a descompressão de um 'Estado' no formato textual utilizado pela função 'comprime'.

== Exemplo de utilização:
>>> descomprime "\n\n,\a?\bV,|\SOH\SOHB\SOH\SOH\SOH\ENQ\SOHD\SOH\SOH\SOH|C\SOH\ENQ"
    Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (5,1), direcaoJogador = D, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 1, tempoDisparo = 5}]}
-}     
descomprime :: String -> Estado
descomprime str = Estado mapa jogador disparos
  where
    (mapaStr,restante) = span (/= '|') str
    (mapa, resto) = (str2mp mapaStr, tail restante)
    (jogstr,dispComSeparador) = span (/= '|') resto
    (jogador, disparosStr) = (stringParaJogadores jogstr, dispComSeparador)
    disparos = if dispComSeparador == "|" then stringParaDisparos [] else stringParaDisparos (tail dispComSeparador)