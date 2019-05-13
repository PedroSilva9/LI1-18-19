{-|
Module      : EstadoInicial
Description : Modulo integrante da Tarefa 5 do projeto a desenvolver no âmbito da unidade curricular LI1.
Copyright   : José Pedro Silva <a84577@uminho.pt>
              Rui Carvalho <a89498@uminho.pt>
Módulo da tarefa 5 que contém o estado inicial do 'EstadoGloss' definido no ficheiro 'Typos.hs'.
-}
module EstadoInicial where

import LI11819
import Typos
import System.Random (mkStdGen)
import Tarefa1_2018li1g027 (mapaInicial, editorInicial, instrucao)

{- | Função que define os valores iniciais dos parametros do 'EstadoGloss'
estado: O estado é definido inicialmente com um mapa de dimensão (6,6), a lista de jogadores é vazia e a lista de disparos vazia.

    @
   * __editor__: O editor usado no construtor de mapas é definido pelo editor inicial no qual se aplica a funcão desenha para que o tetromino inicial seja apresentado no ecrã.

   * __menu__: Inicialmente o valor lógico do parâmetro menu é colocado a @True@ pois o jogo começa no menu inicial.

   * __nJog__: O número de jogadores inicialmente é 0.

   * __tMenu__: Inicialmente o tipo de menu (tMenu) é a primeira imagem da lista das 'imagensMenu', parâmetro de 'Imagens' passada para a função /EditorInicial/.

   * __acoesMapa__: A lista de ações (/instruções/) usadas no criador de mapas, inicialmente, é @vazia@.

   * __criaMapa__: Primeiramente este valor é colocado a @Falso@, pois o criador de mapas é algo que é ativado através do menu.

   * __powerups__: A lista dos powerups é vazia. Á medida que eles são gerados vão sendo acrescentados a esta lista.

   * __powerUpCounter__: Contador usado para saber quantos powerups existem no 'Mapa' e em que 'Posicao'.

   * __rnd__: Semente usada para gerar números aleatórios, é atualizada cada vez que se gera um número aleatório.

   * __empty__: Lista que guarda as 'Posicao' para qual o jogador se pode mover quando usar o 'PowerUpTeleport'.

   * __tickCounter__: Inicialmente este valor é colocado a 0. Este valor é usado para desenhar os ticks no ecrã.

   * __windowSize__: A resolução inicial é igual à dimensão do display inicial. Este parâmetro é usado na função 'verifyWindowSize' para impedir o resize de certos elementos no ecrã
                 como por exemplo as imagens dos estados dos jogadores como forma de evitar a sopresição de algumas imagens impedindo erros visuais.

   * __botIndex__: A lista de bots inicial é vazia. Após a escolha da quantidade de jogadores num jogo, o indice dos jogadores que são bots serão colocados neste parâmetro. A quantidade de
               bots é definida pela diferença entre 4 e a quantidade de tanques jogáveis escolhida pelo utilizador.

   * __winner__: Este valor é definido como -1 sendo que quando um jogador ganha, este é substituido pelo indice do tanque vencedor (único sobrevivente). A função 'ReageTempo' verifica se
             há vencedor e coloca neste parâmetro o indice do vencedor.
   @
-}
estadoI :: Imagens -> EstadoGloss
estadoI a = EstadoGloss
            { estado          = Estado {mapaEstado = mapaInicial (6,6), jogadoresEstado = [],disparosEstado = []}
            , editor          = instrucao Desenha $ editorInicial []
            , menu            = True
            , imagens         = a
            , nJog            = 0
            , tMenu           = head . imagensMenu $ a
            , acoesMapa       = []
            , criaMapa        = False
            , powerups        = []
            , powerUpCounter  = 100
            , rnd             = mkStdGen 5
            , empty           = []
            , tickCounter     = 0
            , windowSize      = (1280,720)
            , botIndex        = []
            , winner          = -1
            }
