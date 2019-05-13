{-|
Module      : Tarefa4_2018li1g027
Description : Tarefa 4 do projeto a desenvolver no âmbito da disciplina de LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>
O objetivo desta tarefa atualizar o estado do jogo a cada tick que passa.
-}

module Tarefa4_2018li1g027 where

import LI11819
import TestesT3 (test)
import TestesT4 (testest4)
import Tarefa2_2018li1g027 (somaVetores)
import Tarefa1_2018li1g027 (atualizaPosicaoMatriz,direcaoParaVetor)
import Data.List (nub,intersect,partition)
import Data.Maybe (mapMaybe)

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = test ++ testest4

{- | Função que transforma uma posição grelha nas respetivas 4 posições normais ocupadas pelo tank.

== Exemplo de utilização:
>>> asSqr (1,1)
    > [(1,1),(2,1),(1,2),(2,2)]   
-}

asSqr :: PosicaoGrelha -> [Posicao]
asSqr (a,b) = [(a,b),(a+1,b),(a,b+1),(a+1,b+1)]

{- | Função que transforma uma posição grelha de um disparo nas respetivas 2 posições normais ocupadas pelo disparo.

== Exemplo de utilização:
>>> asTwo (DisparoCanhao 0 (1,1) D)
    > [(1,2),(2,2)]   
-}
asTwo :: Disparo -> [Posicao]
asTwo d = fst $ shotAsTwoBlocks (posicaoDisparo d) (direcaoDisparo d)

-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers e = atualizaDisparosLaser $ flip verifiesTanks pathNC $ verifiesDestructable rmCanhoes path allD 
  where
    allLasers      = filter (\d -> case d of {DisparoLaser{} -> True; _ -> False}) $ disparosEstado e
    tuploPosDir    = zip (map posicaoDisparo allLasers) (map direcaoDisparo allLasers)
    tuploAsTwoPos  = map (uncurry shotAsTwoBlocks) tuploPosDir
    allPos         = [(l,c) | l <- [0 .. pred . length . mapaEstado $ e], c <- [0 .. pred . length . head . mapaEstado $ e]]
    allPosGroup    = groupN allPos $ length . head . mapaEstado $ e
    (allI, allD)   = (posIndestrutivel (mapaEstado e) allPosGroup, posDestrutivel (mapaEstado e) allPosGroup)
    (pathNC, path) = (map (flip allPosPath allI) tuploAsTwoPos, nub . concat $ pathNC)
    rmCanhoes      = verifiesCanhoes e $ concatMap (flip laserPath allI) tuploPosDir

-- | Função que remove os disparos do estado, uma vez que o disparo tem duração de 1 tick.
atualizaDisparosLaser :: Estado -> Estado
atualizaDisparosLaser e = let newDisp = filter (\d -> case d of {DisparoLaser{} -> False; _ -> True}) $ disparosEstado e
                          in e{disparosEstado = newDisp}

-- | Função que verifica se existe algum canhão no caminho de um laser.
verifiesCanhoes :: Estado -> [Posicao] -> Estado
verifiesCanhoes e l = e{disparosEstado = aux (disparosEstado e) l}
  where
    aux :: [Disparo] -> [Posicao] -> [Disparo]
    aux [] _ = []
    aux (d@(DisparoCanhao _ pos _) : t) l = if pos `elem` l then aux t l else d : aux t l
    aux (h:t) l = h : aux t l

-- | Função que verifica se existe algum tank no caminho de um laser, caso exista remove uma vida.
verifiesTanks :: Estado -> [[Posicao]] -> Estado
verifiesTanks e path = e{jogadoresEstado = map (flip aux path) $ jogadoresEstado e}
  where
    aux :: Jogador -> [[Posicao]] -> Jogador
    aux j [] = j
    aux j (pos:post) = let vidas = vidasJogador j 
                       in if any (flip elem pos) (asSqr . posicaoJogador $ j) && vidas > 0
                             then aux j{vidasJogador = pred vidas} post
                             else aux j post

-- | Função que verifica se existem peças indestrutíveis no caminho de um laser, caso existam, transforma essas peças em vazias.
verifiesDestructable :: Estado -> [Posicao] -> [Posicao] -> Estado
verifiesDestructable e path dest = e{mapaEstado = aux intersection mapa}
  where
    intersection = path `intersect` dest
    mapa = mapaEstado e
    aux :: [Posicao] -> Mapa -> Mapa
    aux [] m = m
    aux p m = foldl (\m h -> atualizaPosicaoMatriz h Vazia m) m p

{- | Função que devolve todas as __posições grelha__ afetadas por um laser.

== Exemplo de utilização:
>>> let mapa = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
        indestrutiveis = [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(1,0),(1,9),(2,0),(2,9),(3,0),(3,9),(4,0),(4,9),(5,0),(5,9),(6,0),(6,9),(7,0),(7,9),(8,0),(8,9),(9,0),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9)] 
    laserPath ((1,1),D) indestrutiveis
    [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9)]   
-}
laserPath :: (PosicaoGrelha,Direcao) -> [Posicao] -> [PosicaoGrelha] 
laserPath (x,C) posInd = [(l,snd x) | l <- [maxRow .. fst x]]
  where
    sameColumn = filter (\(a,b) -> b == snd x && a <= fst x) posInd
    maxRow = maximum $ map fst sameColumn
laserPath (x,D) posInd = [(fst x,c) | c <- [snd x .. minColumn]]
  where
    sameRow = filter (\(a,b) -> a == fst x && b >= snd x) posInd
    minColumn = minimum $ map snd sameRow
laserPath (x,B) posInd = [(l,snd x) | l <- [fst x .. minRow]]
  where
    sameColumn = filter (\(a,b) -> b == snd x && a >= fst x) posInd
    minRow = minimum $ map fst sameColumn
laserPath (x,E) posInd = [(fst x,c) | c <- [maxColumn .. snd x]]
  where
    sameRow = filter (\(a,b) -> a == fst x && b <= snd x) posInd
    maxColumn = maximum $ map snd sameRow

{- | Função que devolve todas as __posições__ afetadas por um laser.

== Exemplo de utilização:
>>> let mapa = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
        indestrutiveis = [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(1,0),(1,9),(2,0),(2,9),(3,0),(3,9),(4,0),(4,9),(5,0),(5,9),(6,0),(6,9),(7,0),(7,9),(8,0),(8,9),(9,0),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9)] 
    allPosPath ([(1,1),(2,1),(1,2),(2,2)],D) indestrutiveis
    [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9)]   
-}
allPosPath :: ([Posicao],Direcao) -> [Posicao] -> [Posicao]
allPosPath (x:(xs:t),C) posInd = [(l,snd x) | l <- [maxRow .. fst x]] ++ [(l,snd xs) | l <- [maxRow .. fst xs]]
  where
    (sameColumnFst, sameColumnSnd) = (filter (\(a,b) -> b == snd x && a <= fst x) posInd, filter (\(a,b) -> b == snd xs && a <= fst x) posInd)
    maxRow = max (maximum $ map fst sameColumnFst) (maximum $ map fst sameColumnSnd)
allPosPath (x:(xs:t),D) posInd = [(fst x,c) | c <- [snd x .. minColumn]] ++ [(fst xs,c) | c <- [snd xs .. minColumn]]
  where
    (sameRowFst, sameRowSnd) = (filter (\(a,b) -> a == fst x && b >= snd x) posInd, filter (\(a,b) -> a == fst xs && b >= snd x) posInd)
    minColumn = min (minimum $ map snd sameRowFst) (minimum $ map snd sameRowSnd)
allPosPath (x:(xs:t),B) posInd = [(l,snd x) | l <- [fst x .. minRow]] ++ [(l,snd xs) | l <- [fst xs .. minRow]]
  where
    (sameColumnFst, sameColumnSnd) = (filter (\(a,b) -> b == snd x && a >= fst x) posInd, filter (\(a,b) -> b == snd xs && a >= fst x) posInd)
    minRow = min (minimum $ map fst sameColumnFst) (minimum $ map fst sameColumnSnd)
allPosPath (x:(xs:t),E) posInd = [(fst x,c) | c <- [maxColumn .. snd x]] ++ [(fst xs,c) | c <- [maxColumn .. snd xs]]
  where
    (sameRowFst, sameRowSnd) = (filter (\(a,b) -> a == fst x && b <= snd x) posInd, filter (\(a,b) -> a == fst xs && b <= snd x) posInd)
    maxColumn = max (maximum $ map snd sameRowFst) (maximum $ map snd sameRowSnd)

-- | Função que recebendo uma posição grelha e uma direção devolve um tuplo formado pelas duas posições necessárias para verificar os embates e a direção.
shotAsTwoBlocks :: PosicaoGrelha -> Direcao -> ([Posicao], Direcao)
shotAsTwoBlocks (li,co) C = ([(li,co), (li,succ co)], C)
shotAsTwoBlocks (li,co) B = ([(succ li, co), (succ li, succ co)], B)
shotAsTwoBlocks (li,co) D = ([(li, succ co), (succ li, succ co)], D)
shotAsTwoBlocks (li,co) E = ([(li,co), (succ li,co)], E)

-- | Função que recebendo uma lista e um inteiro /x/ agrupa a lista com /x/ elementos.
groupN :: [a] -> Int -> [[a]]
groupN [] _ = []
groupN l n = take n l : groupN (drop n l) n

-- | Função que devolve todas as posições de um mapa que sejam __Indestrutíveis__.
posIndestrutivel :: Mapa -> [[Posicao]] -> [Posicao]
posIndestrutivel [] [] = []
posIndestrutivel (x:xs) (y:ys) = aux x y ++ posIndestrutivel xs ys
  where
    aux :: [Peca] -> [Posicao] -> [Posicao]
    aux [] [] = []
    aux (x:xs) (y:ys) = if x == Bloco Indestrutivel then y : aux xs ys else aux xs ys

-- | Função que devolve todas as posições de um mapa que sejam __Destrutíveis__.
posDestrutivel :: Mapa -> [[Posicao]] -> [Posicao]
posDestrutivel [] [] = []
posDestrutivel (x:xs) (y:ys) = aux x y ++ posDestrutivel xs ys
  where
    aux :: [Peca] -> [Posicao] -> [Posicao]
    aux [] [] = []
    aux (x:xs) (y:ys) = if x == Bloco Destrutivel then y : aux xs ys else aux xs ys

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes est = let newEstado = verifiesDestructable e{disparosEstado = removeInd ++ resto} shotsAsTwoPos allD
                      disps = intersect (disparosEstado newEstado) (disparosEstado e)
                  in newEstado{disparosEstado = map avancaDisparo disps}
  where
    (all, resto)  = partition (\d -> case d of {DisparoCanhao{} -> True; _ -> False}) $ disparosEstado est
    (new, e)      = (checkTanks est all, est{jogadoresEstado = jogadoresEstado new, disparosEstado = disparosEstado new})
    tuploPosDir   = zip (map posicaoDisparo all) (map direcaoDisparo all)
    shotsAsTwoPos = concatMap (fst . uncurry shotAsTwoBlocks) tuploPosDir
    allPos        = [(l,c) | l <- [0 .. pred . length . mapaEstado $ e], c <- [0 .. pred . length . head . mapaEstado $ e]]
    allPosGroup   = groupN allPos $ length . head . mapaEstado $ e
    (allI, allD)  = (posIndestrutivel (mapaEstado e) allPosGroup, posDestrutivel (mapaEstado e) allPosGroup)
    disparos      = removeSobrepostos (map posicaoDisparo all) $ justPassed all
    removeDest    = removeCanhao disparos $ intersect shotsAsTwoPos allD
    removeInd     = removeCanhao removeDest $ intersect shotsAsTwoPos allI

-- | Função que avança uma unidade na direção dos disparos, todos os diparos do tipo 'Canhao'.
avancaDisparo :: Disparo -> Disparo
avancaDisparo d@(DisparoCanhao _ pos dir) = let newPos = somaVetores pos $ direcaoParaVetor dir
                                            in d{posicaoDisparo = newPos}
avancaDisparo d = d 

-- | Função que ao receber um 'Estado' e uma lista de 'Disparo's verifica se alguma bala embate com um tank. 
checkTanks :: Estado -> [Disparo] -> Estado
checkTanks e lDisp = e{jogadoresEstado = newJog, disparosEstado = newDisp}
  where
    (players,disparos) = (jogadoresEstado e, filter (\d -> case d of {DisparoCanhao{} -> False; _ -> True}) $ disparosEstado e)
    (newJog,disp) = (atualizaJog players lDisp, atualizaDisp players lDisp)
    newDisp = disparos ++ disp

-- | Função verifica se existe algum 'Canhao' a embater num 'Jogador'.
atualizaJog :: [Jogador] -> [Disparo] -> [Jogador]
atualizaJog j d = map (flip atualizaJogAux (map asTwo d)) j

-- | Função auxiliar da função 'atualizaJog'.
atualizaJogAux :: Jogador -> [[Posicao]] -> Jogador
atualizaJogAux j [] = j
atualizaJogAux j (lp:lps) = if or intersected && (vidasJogador j /= 0) 
                               then atualizaJogAux (removeVidas j) lps 
                               else atualizaJogAux j lps
  where
    intersected = map (flip elem lp) asTank
    asTank = asSqr . posicaoJogador $ j
    removeVidas :: Jogador -> Jogador
    removeVidas j = let vidas = vidasJogador j
                    in if vidas /= 0
                          then j{vidasJogador = pred vidas}
                          else j

-- | Função verifica se existe algum 'Canhao' a embater noutro 'Canhao'.
atualizaDisp :: [Jogador] -> [Disparo] -> [Disparo]
atualizaDisp j d = mapMaybe (aux allJPos) d
  where
    comVida = filter (\j -> vidasJogador j /= 0) j
    allJPos = concatMap (asSqr . posicaoJogador) comVida
    aux :: [Posicao] -> Disparo -> Maybe Disparo
    aux allJPos d = let dispPos = fst $ shotAsTwoBlocks (posicaoDisparo d) (direcaoDisparo d)
                    in if any (flip elem allJPos) dispPos
                          then Nothing
                          else Just d

-- | Função que remove todos os disparos 'Canhao' que embateram numa 'Parede'.
removeCanhao :: [Disparo] -> [Posicao] -> [Disparo]
removeCanhao lDisp l = mapMaybe (aux l) lDisp
  where
    aux :: [Posicao] -> Disparo -> Maybe Disparo
    aux l d = if any (flip elem l) $ asTwo d then Nothing else Just d

-- | Função que remove os disparos 'Canhao' que acabaram de passar um pelo outro.
justPassed :: [Disparo] -> [Disparo]
justPassed lDisps = mapMaybe (aux tuploShot) lDisps
  where
    oppositeDirMap = map (toEnum . flip mod 4 . (+) 2 . fromEnum . direcaoDisparo) lDisps
    oppositePosMap = zipWith somaVetores (map posicaoDisparo lDisps) (map direcaoParaVetor oppositeDirMap)
    tuploShot      = zip oppositePosMap oppositeDirMap
    aux :: [(Posicao,Direcao)] -> Disparo -> Maybe Disparo
    aux tuploOpposite disp = let tuplo = (posicaoDisparo disp, direcaoDisparo disp)
                             in if tuplo `elem` tuploOpposite then Nothing else Just disp

-- | Função que remove os disparos 'Canhao' que estão na mesma posição.
removeSobrepostos :: [PosicaoGrelha] -> [Disparo] -> [Disparo]
removeSobrepostos allPos lDisp = mapMaybe (sameSpot allPos) lDisp
  where
    sameSpot :: [PosicaoGrelha] -> Disparo -> Maybe Disparo
    sameSpot allPos disp = let dispPos = posicaoDisparo disp
                           in if length (filter (== dispPos) allPos) > 1 then Nothing else Just disp
                        
-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques e = e{disparosEstado = nChoques ++ newTicks}
  where
    (choques, nChoques) = partition (\d -> case d of {DisparoChoque{} -> True; _ -> False}) $ disparosEstado e
    newTicks            = mapMaybe atualizaTicks choques

-- | Função que atualiza os ticks de um 'Choque', caso o número de ticks seja 0, remove o 'Choque'.
atualizaTicks :: Disparo -> Maybe Disparo
atualizaTicks d = let tick = tempoDisparo d 
                  in if tick == 0
                        then Nothing
                        else Just d{tempoDisparo = pred tick}
