----------------------------------------------------------------------------------------------------------------------------
-- 										Trabalho de linguagem de programação:												                                         -
--																															                                                           -  
-- 	Tema: Implementação do campo minado em Haskell																			                                   -
--																															                                                           -
--	Nome: Lucas Diniz da Costa		Matricula: 201465524AC																	                                 -
-- 																															                                                           -
-----------------------------------------------------------------------------------------------------------------------------

module CampoMinado where

import Graphics.UI.Gtk
import Data.Array
import Data.List
import System.IO
import System.Random
import System.Process           -- Para poder ativar a limpeza do console
import System.IO.Unsafe
import Control.Exception
import System.IO.Error
import Data.Char                -- Algumas funções interessantes de caracteres



minas = 8
linhas = 4
colunas = 4


-- definição dos tipos dos dados

type IdLinha = Int
type IdColuna = Int
type Fechado = Bool                  -- Contrasta os vizinhos e não pode mais ser visitado
type Mina = Bool                     -- A celula possui mina
type MarcacaoMinaJogador = Bool      -- False => não tem mina marcada pelo jogador, True => tem minha marcada pelo jogador
type Vizinho = Int                   -- Quantidade de vizinhos com minas
type Escrito = String                -- Escrito da celula que mostra o estado dela, por padrão é '*'
data Celula = Celula Escrito IdLinha IdColuna Fechado Mina MarcacaoMinaJogador Vizinho
                              deriving(Show, Eq) -- Possibilita ao tipo de dado a utilização dos métodos de show e Eq

{-
    FUNÇÕES AUXILIARES DE EMBARALHAMENTO E ALEATORIEDADE
-}

numAleatorio:: Int -> Int -> IO(Int)
numAleatorio ini fim = randomRIO (ini, fim)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

posicaoListaAleatoria:: [Int] -> IO Int
posicaoListaAleatoria xs = getStdRandom (randomR (0, length xs - 1))

-- Retira o "IO a" para o tipo padrão do dado
transform::IO a -> a
transform x = unsafePerformIO x

listaEmbaralhada:: [a] -> [a]
listaEmbaralhada lis = transform(shuffle lis)


-- Controle de estado
estadoInterface = 0


{-
    FUNÇÕES DE TRABALHO COM INTERFACE
-}

main :: IO ()
main = do
  initGUI

  -- Cria uma nova janela
  window <- windowNew

  -- Conecta ao evento de "destruição" da janela.
  -- O evento ocorre quando é chamado o "widgetDestroy" na janela,
  -- ou seo usuário fecha a janela.

  window `onDestroy` mainQuit

  -- Define os parametros da janela
  set window [ windowDefaultWidth := 300, windowDefaultHeight := 300,
               windowTitle := "Campo Minado", containerBorderWidth := 20]

  window `on` focus $ \dirtype -> putStrLn "Janela criada!" >> return False

  --removeIndiceLista :: Int -> [a] -> [a]
  --removeIndiceLista n lista = delete (lista!!n) lista

  -- Cria um agrupamento de botões
  hbuttonbox <- hButtonBoxNew

  set window [ containerChild := hbuttonbox ]

  button1 <- buttonNewWithLabel "Console"
  button2 <- buttonNewWithLabel "Interface Gráfica"
  button3 <- buttonNewWithLabel "Three"

  onClicked button1 (prepararJogo)  -- Atribuindo função a um botão
  onClicked button2 (print"Funcionou")  -- Atribuindo função a um botão
  --onClicked button1 $ do
  --onClicked button3 $ do
    --          teste
            --(print(removeIndiceLista 0 listaTeste))

  -- Add each button to the button box with the default packing and padding
  set hbuttonbox [ containerChild := button
                 | button <- [button1, button2, button3] ]

  -- This sets button3 to be a so called 'secondary child'. When the layout
  -- stlye is ButtonboxStart or ButtonboxEnd, the secondary children are
  -- grouped seperately from the others. Resize the window to see the effect.
  --
  -- This is not interesting in itself but shows how to set child attributes.
  -- Note that the child attribute 'buttonBoxChildSecondary' takes the
  -- button box container child 'button3' as a parameter.
  set hbuttonbox [ buttonBoxLayoutStyle := ButtonboxStart
                 , buttonBoxChildSecondary button3 := True ]

  -- The final step is to display everything (the window and all the widgets
  -- contained within it)
  widgetShowAll window

  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  mainGUI


{-
    FUNCIONALIDADES DO CAMPO MINADO
-}

-- função que prepara o início do jogo
prepararJogo:: IO()
prepararJogo = do
               system "clear"     -- no windows eh 'system "cls"'
               putStrLn "\n--------------------------------------------------------------------------\n"
               putStrLn "------ Trabalho de Linguagem de programação (2019/1):   ------------------\n"
               putStrLn "------ Aluno: Lucas Diniz da Costa                      ------------------\n"
               putStrLn "--------------------------------------------------------------------------\n"
               putStrLn "--------------------------------------------------------------------------\n"
               putStrLn "-------------------            CAMPO MINADO           --------------------\n"
               putStrLn "--------------------------------------------------------------------------\n\n"
               putStr "Digite o numero de linhas: "
               linhas <- getLine
               putStr "Digite o numero de colunas: "
               colunas <- getLine
               putStrLn (linhas ++ " " ++ colunas ++ "\n")
               minas <- (tratarMinas(read (linhas):: Int) (read(colunas):: Int))
               putStrLn (linhas ++ " " ++ colunas ++ " " ++ (show(minas)) ++ "\n")
               let qtdCelulasVazias = ((read (linhas)::Int) * (read (colunas)::Int) - minas)
               campoMinado <- montarMapa((read (linhas):: Int), (read (colunas):: Int), minas, qtdCelulasVazias)
               executarJogada(campoMinado, read (linhas)::Int, read (colunas)::Int, minas, minas, qtdCelulasVazias)
               putStr ""   -- Se tirar causa erro de identação

-- executarJogada : Exibe o mapa e instruções pra executar a jogada
executarJogada::([Celula], Int, Int, Int, Int, Int) -> IO()
executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias) = do
                      printMapa(linhas, colunas, minasMapa, minasJogador, mapa, qtdCelulasVazias)
                      putStr "\tJogadas possiveis: \n\n"
                      putStrLn("=> |  posicao | posicao a ser aberta                 | Exemplo:  A1 |  ")
                      putStrLn("=> | +posicao | posicao marcada como mina            | Exemplo: +D2 |  ")
                      putStrLn("=> | -posicao | desmarcar posicao marcada como mina  | Exemplo: -D2 |\n")
                      putStr("Digite sua jogada: ")
                      jogada <- getLine
                      tratarJogada(jogada, linhas, colunas, minasMapa, minasJogador, mapa, qtdCelulasVazias)
                      putStr("")

-- terminoPartida : Solicitar ao jogador se ele quer jogar de novo
terminaPartida:: IO ()
terminaPartida = do
                 putStr "Deseja jogar novamente('S'/'s','N'/'n'): "
                 jogarDeNovo <- getChar
                 putStr("\n")
                 if((jogarDeNovo == 'S')||(jogarDeNovo == 's'))
                    then do
                      putStrLn("Pressione ENTER pra continuar!")
                      getChar -- descarta o Enter
                      prepararJogo
                      putStr("")
                    else do
                      if((jogarDeNovo == 'N')||(jogarDeNovo == 'n'))
                        then do
                          putStrLn "\n\n--------------------------------------------------------------------------\n"
                          putStrLn "--------------------------------------------------------------------------\n\n"
                          return()
                        else do
                          putStr "Digite S para jogar novamente e N para sair!!!\n"
                          putStrLn("Pressione ENTER pra continuar!")
                          getChar -- descarta o Enter
                          terminaPartida
                          putStr("")

tratarJogada:: (String, Int, Int, Int, Int, [Celula], Int) -> IO()
tratarJogada(jogada, linhas, colunas, minasMapa, minasJogador, mapa, qtdCelulasVazias) = 
    do
      if(length(jogada) >= 2)
        then do
          let priPosicao = ord(jogada!!0)
          let segPosicao = ord(jogada!!1)  -- CODIGOS DOS CARACTERES
          if((priPosicao >= 65) && (priPosicao <= 90))   -- São as letras maisculas
            then do
              testeNumerico <- (analisaStringNumero(retornaSubstring(jogada, 0, 1)))  -- Remove o caractere da letra e faz a analise se é numero Se nao for, o valor é -100, o contrário é valor corretamente
              if(testeNumerico >= 0)
                then do
                  -- VALOR CORRETO
                  codLinha <- (analisaStringNumero(retornaSubstring(jogada, 0, 1))) -- Remove a letra
                  let codColuna = (priPosicao - 65)   -- menos a posicao do 'A'
                  if(testeIndiceValido(codLinha, codColuna, linhas, colunas))
                    then do
                      let celulaMapa = (retornaCelulaPelaMatriz(codLinha, codColuna, mapa))
                      if(obterFechado(celulaMapa))
                        then do
                          putStrLn("A celula ja esta fechada, escolha outra celula para abrir!")
                          putStrLn("Pressione ENTER pra continuar!")
                          getChar -- descarta o Enter
                          executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                        else do
                          if(obterMarcacaoMinaJogador(celulaMapa)) -- Tentativa de abrir uma celula marcada como mina pelo jogador
                            then do
                              putStrLn("A celula ja esta marcada como mina pelo jogador, escolha outra celula para abrir\nou desmarque esta!")
                              putStrLn("Pressione ENTER pra continuar!")
                              getChar -- descarta o Enter
                              executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                            else do
                              if(obterEhMina(celulaMapa))
                                then do                       -- DERROTA
                                  mapa <- (revelarMapa(mapa, [], linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias))
                                  printMapa(linhas, colunas, minasMapa, minasJogador, mapa, qtdCelulasVazias)
                                  putStrLn("A celula tinha uma mina, infelizmente voce perdeu.")
                                  putStrLn("\n\n \t \t GAME OVER !!! \n\n")
                                  putStrLn("Pressione ENTER pra continuar!")
                                  getChar -- descarta o Enter
                                  terminaPartida
                                else do                       -- Jogada válida
                                  if(qtdCelulasVazias > 1)
                                    then do
                                      mapa <- (atualizaMapa(mapa, [], celulaMapa, linhas, colunas, minasMapa, minasJogador, (qtdCelulasVazias - 1), 0))
                                      executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, (qtdCelulasVazias - 1))
                                    else do                   -- VITORIA
                                      mapa <- (atualizaMapa(mapa, [], celulaMapa, linhas, colunas, minasMapa, minasJogador, (qtdCelulasVazias - 1), 0))
                                      mapa <- (revelarMapa(mapa, [], linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias))
                                      printMapa(linhas, colunas, minasMapa, minasJogador, mapa, qtdCelulasVazias)
                                      putStrLn("\n\n \t \tVocê venceu, parabéns!!!!")
                                      putStrLn("\t \tVocê venceu, parabéns!!!! \n\n")
                                      putStrLn("Pressione ENTER pra continuar!")
                                      getChar -- descarta o Enter
                                      terminaPartida
                    else do
                      -- PARAMETROS ERRADOS
                      putStr("\n\nErro na passagem de parametros, Indices invalidos.\n")
                      putStrLn("Pressione ENTER pra continuar!")
                      getChar -- descarta o Enter
                      executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                else do
                  putStrLn("Erro na passagem de parametros, utilize indices validos")
                  putStrLn("Pressione ENTER pra continuar!")
                  getChar -- descarta o Enter
                  executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
            else do
              if(length(jogada) >= 3) -- Simbolo, letra, Numero
                then do
                  let priPosicao = ord(jogada!!0)
                  let segPosicao = ord(jogada!!1)  -- CODIGOS DOS CARACTERES
                  if(priPosicao == 43)  -- '+' => Marcar mina
                    then do
                      if((segPosicao >= 65) && (segPosicao <= 90))-- São as letras maisculas
                        then do
                          testeNumerico <- (analisaStringNumero(retornaSubstring(jogada,0,2)))
                          if(testeNumerico >= 0)
                            then do
                              codLinha <- (analisaStringNumero(retornaSubstring(jogada,0,2))) -- Remove a letra e o simbolo
                              let codColuna = (segPosicao - 65)   -- menos a posicao do 'A'
                              if(testeIndiceValido(codLinha, codColuna, linhas, colunas))
                                then do
                                  let celulaMapa = (retornaCelulaPelaMatriz(codLinha,codColuna,mapa))
                                  if(obterFechado(celulaMapa))
                                    then do
                                      putStrLn("\n\nA celula ja esta fechada, escolha outra celula para marcar como mina!")
                                      putStrLn("Pressione ENTER pra continuar!")
                                      getChar -- descarta o Enter
                                      executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                                    else do
                                      if(obterMarcacaoMinaJogador(celulaMapa) == False) -- Tenta Marcar célula já marcada como mina
                                        then do
                                          if(minasJogador <= 0)
                                            then do
                                              putStrLn("\n\nA quantidade de marcações de minas acabaram, remova de outras celulas se quiser marcar esta!")
                                              putStrLn("Pressione ENTER pra continuar!")
                                              getChar -- descarta o Enter
                                              executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                                            else do
                                              mapa <- (atualizaMapa(mapa, [], celulaMapa, linhas, colunas, minasMapa, (minasJogador - 1), qtdCelulasVazias, 1))
                                              putStrLn("\nA celula foi marcada como mina!")
                                              putStrLn("Pressione ENTER pra continuar!")
                                              getChar -- descarta o Enter
                                              executarJogada(mapa, linhas, colunas, minasMapa, (minasJogador - 1), qtdCelulasVazias)
                                        else do
                                          putStrLn("\n\nA celula ja esta marcada como mina pelo jogador, escolha outra celula para marcar ou desmarque esta!")
                                          putStrLn("Pressione ENTER pra continuar!")
                                          getChar -- descarta o Enter
                                          executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                                else do
                                  -- INDICES INVALIDOS
                                  putStr("\n\nErro na passagem de parametros, indices invalidos!\n")
                                  putStrLn("Pressione ENTER pra continuar!")
                                  getChar -- descarta o Enter
                                  executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                            else do
                              --CARACTERE INCORRETO APOS O SIMBOLO E A LETRA
                              putStr("\n\nErro na passagem de parametros, caracteres incorretos após a letra\n favor olhar os exemplos e preencha corretamente!\n")
                              putStrLn("Pressione ENTER pra continuar!")
                              getChar -- descarta o Enter
                              executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                        else do
                          putStr("\n\nErro na passagem de parametros!")
                          putStrLn("Pressione ENTER pra continuar!")
                          getChar -- descarta o Enter
                          executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                    else do
                      if(priPosicao == 45)  -- '-' => Desmarcar mina do jogador
                        then do
                          if((segPosicao >= 65) && (segPosicao <= 90))-- São as letras maisculas
                            then do
                              testeNumerico <- (analisaStringNumero(retornaSubstring(jogada,0,2))) 
                              if(testeNumerico >= 0)
                                then do
                                  codLinha <- (analisaStringNumero(retornaSubstring(jogada,0,2))) -- Remove a letra e o simbolo
                                  let codColuna = (segPosicao - 65)   -- menos a posicao do 'A'
                                  if(testeIndiceValido(codLinha, codColuna, linhas, colunas))
                                    then do
                                      let celulaMapa = retornaCelulaPelaMatriz(codLinha,codColuna,mapa)
                                      if(obterFechado(celulaMapa))
                                        then do
                                          putStrLn("\n\nA celula ja esta fechada, escolha celula marcada para desmarcar como mina!")
                                          putStrLn("Pressione ENTER pra continuar!")
                                          getChar -- descarta o Enter
                                          executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                                        else do
                                          if(minasJogador < minasMapa) -- Tenta desmarcar uma celula sem marcação
                                            then do
                                              if(obterMarcacaoMinaJogador(celulaMapa))
                                                then do
                                                  mapa <- (atualizaMapa(mapa, [], celulaMapa, linhas, colunas, minasMapa, (minasJogador + 1), qtdCelulasVazias, 2))
                                                  putStrLn("\n\nA celula foi desmarcada!")
                                                  putStrLn("Pressione ENTER pra continuar!")
                                                  getChar -- descarta o Enter
                                                  executarJogada(mapa, linhas, colunas, minasMapa, (minasJogador + 1), qtdCelulasVazias)
                                                else do
                                                  putStrLn("\n\nA celula nao possui marcacao de mina, por favor escolha outra celula para desmarcar!")
                                                  putStrLn("Pressione ENTER pra continuar!")
                                                  getChar -- descarta o Enter
                                                  executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                                            else do
                                              putStrLn("\n\nTodas as celulas foram desmarcas como minas!")
                                              putStrLn("Pressione ENTER pra continuar!")
                                              getChar -- descarta o Enter
                                              executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                                    else do
                                      -- INDICES INVALIDOS
                                      putStr("\n\nErro na passagem de parametros, indices invalidos!\n")
                                      putStrLn("Pressione ENTER pra continuar!")
                                      getChar -- descarta o Enter
                                      executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                                else do
                                  --CARACTERE INCORRETO APOS O SIMBOLO E A LETRA
                                  putStr("\n\nErro na passagem de parametros, caracteres incorretos após a letra\n favor olhar os exemplos e preencha corretamente!\n")
                                  putStrLn("Pressione ENTER pra continuar!")
                                  getChar -- descarta o Enter
                                  executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                            else do
                              putStr("\n\nErro na passagem de parametros, simbolos invalidos!")
                              putStrLn("Pressione ENTER pra continuar!")
                              getChar -- descarta o Enter
                              executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                        else do
                          putStr("\n\nErro na passagem de parametros, simbolos invalidos!")
                          putStrLn("Pressione ENTER pra continuar!")
                          getChar -- descarta o Enter
                          executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                else do
                   putStr("\n\nErro na passagem de parametros, simbolos invalidos!")
                   putStrLn("Pressione ENTER pra continuar!")
                   getChar -- descarta o Enter
                   executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
         else do
          putStr("\n\nErro na passagem de parametros, simbolos invalidos!")
          putStrLn("Pressione ENTER pra continuar!")
          getChar -- descarta o Enter
          executarJogada(mapa, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)

-- Retorna se os indices passados são válidos em relação ao campo minado
testeIndiceValido::(Int, Int, Int, Int) -> Bool
testeIndiceValido(i, j, linhas, colunas) = 
                          do
                            if(((i >= 0) && (i < linhas)) && ((j >= 0) && (j < colunas)))
                              then do
                                True
                              else do
                                False


-- Retorna a conversão de uma String pra Int e caso ocorra erro retorna -100.
analisaStringNumero:: String -> IO Int
analisaStringNumero(texto) = do
  eVal <- try (print(read(texto)::Int)) :: IO (Either SomeException ())
  case eVal of
    Right () -> do  -- Nenhuma exceção lançada, conseguiu efetuar a conversão de tipos
                  return(read(texto)::Int)
    Left e   -> do  -- Exceção capturada 
                  putStrLn("Exceção encontrada")
                  return(-100)                     

-- Retorna substring a partir de determinada posicao
-- parada: Caracteres a serem removidos da String original
retornaSubstring:: (String, Int, Int) -> String
retornaSubstring(texto, indice, parada) = do
        if(indice >= parada)
          then do 
            texto;
          else do
            retornaSubstring(tail(texto), (indice + 1), parada)


-- Metodo responsavel por atualizar o mapa com base na alteracao do jogador
atualizaMapa::([Celula], [Celula], Celula, Int, Int, Int, Int, Int, Int) -> IO [Celula]
atualizaMapa((cabeca:mapa), mapaAtualizado, elemento, linhas, colunas, minasMapa, 
            minasJogador, qtdCelulasVazias, 0) = -- opcao da Jogada 1: Abrir a celula
                do
                  if(length(mapa) > 0)    
                    then do
                      --print(show(cabeca)++ " ---  " ++ show(elemento))
                      if((obterIDLinha(cabeca) == obterIDLinha(elemento)) && (obterIDColuna(cabeca) == obterIDColuna(elemento)))
                        then do -- //ALTERAR O ELEMENTO
                          let aux = (mapaAtualizado ++ [(Celula (show(obterVizinho(elemento))) (obterIDLinha(elemento)) (obterIDColuna(elemento)) True (obterEhMina(elemento)) (obterMarcacaoMinaJogador(elemento)) (obterVizinho(elemento)))])
                          atualizaMapa(mapa, aux, elemento, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias, 0)
                        else do --                           //ACHOU FAZ TEMPO -- coloca o resto de mapa no atualizado
                          --print(show(cabeca))
                          atualizaMapa(mapa, (mapaAtualizado ++ [cabeca]), elemento, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias, 0)
                    else do       -- Trata o problema com o ultimo elemento do mapa
                      if((obterIDLinha(cabeca) == obterIDLinha(elemento)) && (obterIDColuna(cabeca) == obterIDColuna(elemento)))
                        then do 
                          let aux = (mapaAtualizado ++ [(Celula (show(obterVizinho(elemento))) (obterIDLinha(elemento)) (obterIDColuna(elemento)) True (obterEhMina(elemento)) (obterMarcacaoMinaJogador(elemento)) (obterVizinho(elemento)))])
                          return(aux)
                        else do --                           
                          --print(show(cabeca)++ " ---  " ++ show(elemento))
                          return(mapaAtualizado ++ [cabeca])

atualizaMapa((cabeca:mapa), mapaAtualizado, elemento, linhas, colunas, minasMapa, 
            minasJogador, qtdCelulasVazias, 1) = -- opcao da Jogada 2: '+' => Colocar mina 
                do
                  if(length(mapa) > 0)    
                    then do
                      --print(show(cabeca)++ " ---  " ++ show(elemento))
                      if((obterIDLinha(cabeca) == obterIDLinha(elemento)) && (obterIDColuna(cabeca) == obterIDColuna(elemento)))
                        then do -- //ALTERAR O ELEMENTO
                          let aux = (mapaAtualizado ++ [(Celula "B" (obterIDLinha(elemento)) (obterIDColuna(elemento)) (obterFechado(cabeca)) (obterEhMina(elemento)) True (obterVizinho(elemento)))])
                          atualizaMapa(mapa, aux, elemento, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias, 1)
                        else do --                           //ACHOU FAZ TEMPO -- coloca o resto de mapa no atualizado
                          --print(show(cabeca))
                          atualizaMapa(mapa, (mapaAtualizado ++ [cabeca]), elemento, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias, 1)
                    else do       -- Trata o problema com o ultimo elemento do mapa
                      if((obterIDLinha(cabeca) == obterIDLinha(elemento)) && (obterIDColuna(cabeca) == obterIDColuna(elemento)))
                        then do 
                          let aux = (mapaAtualizado ++ [(Celula "B" (obterIDLinha(elemento)) (obterIDColuna(elemento)) (obterFechado(cabeca)) (obterEhMina(elemento)) True (obterVizinho(elemento)))])
                          return(aux)
                        else do --                           
                          --print(show(cabeca)++ " ---  " ++ show(elemento))
                          return(mapaAtualizado ++ [cabeca])

atualizaMapa((cabeca:mapa), mapaAtualizado, elemento, linhas, colunas, minasMapa, 
            minasJogador, qtdCelulasVazias, 2) = -- opcao da Jogada 3: '-' => Retirar mina
                do
                  if(length(mapa) > 0)    
                    then do
                      --print(show(cabeca)++ " ---  " ++ show(elemento))
                      if((obterIDLinha(cabeca) == obterIDLinha(elemento)) && (obterIDColuna(cabeca) == obterIDColuna(elemento)))
                        then do -- //ALTERAR O ELEMENTO
                          let aux = (mapaAtualizado ++ [(Celula "*" (obterIDLinha(elemento)) (obterIDColuna(elemento)) (obterFechado(cabeca)) (obterEhMina(elemento)) False (obterVizinho(elemento)))])
                          atualizaMapa(mapa, aux, elemento, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias, 2)
                        else do --                           //ACHOU FAZ TEMPO -- coloca o resto de mapa no atualizado
                          --print(show(cabeca))
                          atualizaMapa(mapa, (mapaAtualizado ++ [cabeca]), elemento, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias, 2)
                    else do       -- Trata o problema com o ultimo elemento do mapa
                      if((obterIDLinha(cabeca) == obterIDLinha(elemento)) && (obterIDColuna(cabeca) == obterIDColuna(elemento)))
                        then do 
                          let aux = (mapaAtualizado ++ [(Celula "*" (obterIDLinha(elemento)) (obterIDColuna(elemento)) (obterFechado(cabeca)) (obterEhMina(elemento)) False (obterVizinho(elemento)))])
                          return(aux)
                        else do --                           
                          --print(show(cabeca)++ " ---  " ++ show(elemento))
                          return(mapaAtualizado ++ [cabeca])


-- Metodo responsavel por revelar o mapa com base no final da partida
revelarMapa::([Celula], [Celula], Int, Int, Int, Int, Int) -> IO [Celula]
revelarMapa((cabeca:mapa), mapaAtualizado, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias) =
                do
                  if(length(mapa) > 0)    
                    then do
                      --print(show(cabeca)++ " ---  " ++ show(elemento))
                      if(obterFechado(cabeca))
                        then do
                          let aux = (mapaAtualizado ++ [(Celula (show(obterVizinho(cabeca))) (obterIDLinha(cabeca)) (obterIDColuna(cabeca)) (obterFechado(cabeca)) (obterEhMina(cabeca)) (obterMarcacaoMinaJogador(cabeca)) (obterVizinho(cabeca)))])
                          revelarMapa(mapa, aux, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                        else do
                          if(obterEhMina(cabeca))
                            then do
                              let aux = (mapaAtualizado ++ [(Celula "B" (obterIDLinha(cabeca)) (obterIDColuna(cabeca)) (obterFechado(cabeca)) (obterEhMina(cabeca)) (obterMarcacaoMinaJogador(cabeca)) (obterVizinho(cabeca)))])
                              revelarMapa(mapa, aux, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                            else do
                              let aux = (mapaAtualizado ++ [(Celula (show(obterVizinho(cabeca))) (obterIDLinha(cabeca)) (obterIDColuna(cabeca)) (obterFechado(cabeca)) (obterEhMina(cabeca)) (obterMarcacaoMinaJogador(cabeca)) (obterVizinho(cabeca)))])
                              revelarMapa(mapa, aux, linhas, colunas, minasMapa, minasJogador, qtdCelulasVazias)
                    else do       -- Trata o problema com o ultimo elemento do mapa
                      if(obterFechado(cabeca))
                        then do
                          let aux = (mapaAtualizado ++ [(Celula (show(obterVizinho(cabeca))) (obterIDLinha(cabeca)) (obterIDColuna(cabeca)) (obterFechado(cabeca)) (obterEhMina(cabeca)) (obterMarcacaoMinaJogador(cabeca)) (obterVizinho(cabeca)))])
                          return(aux)
                        else do
                          if(obterEhMina(cabeca))
                            then do
                              let aux = (mapaAtualizado ++ [(Celula "B" (obterIDLinha(cabeca)) (obterIDColuna(cabeca)) (obterFechado(cabeca)) (obterEhMina(cabeca)) (obterMarcacaoMinaJogador(cabeca)) (obterVizinho(cabeca)))])
                              return(aux)
                            else do
                              let aux = (mapaAtualizado ++ [(Celula (show(obterVizinho(cabeca))) (obterIDLinha(cabeca)) (obterIDColuna(cabeca)) (obterFechado(cabeca)) (obterEhMina(cabeca)) (obterMarcacaoMinaJogador(cabeca)) (obterVizinho(cabeca)))])
                              return(aux)
                          


montarMapa::(Int, Int, Int, Int) -> IO [Celula]
montarMapa(linhas, colunas, minas, qtdCelulasVazias) = do
                   putStrLn "montar Mapa:"
                   let mapa = [(Celula "*" r c False False False 0) | r <- [0..(linhas - 1)] , c <- [0..(colunas - 1)]]--let mapa = [(Celula "*" x y False False False 0)];
                   mapa <- posicionarMinas(0, (linhas*colunas), 0, minas, [], mapa, [])
                   mapa <- calculaVizinhos(0, (linhas*colunas), linhas, colunas, mapa, mapa, [])
                   printMapa(linhas, colunas, minas, 0, mapa, qtdCelulasVazias)
                   return(mapa)   -- Retornando uma lista de células

printMapa:: (Int, Int, Int, Int, [Celula], Int) -> IO()
printMapa(linhas, colunas, minasMapa, minasJogador, mapa, qtdCelulasVazias) = do
                       system "clear"     -- no windows eh 'system "cls"'
                       --print(show(length(mapa))++ " --  elementos na matriz.")
                       putStrLn "Imprimindo mapa:"
                       putStrLn "\n--------------------------------------------------------------------------\n"
                       forLoopPrintMapa(0, (linhas*colunas), linhas, colunas, 0, mapa)
                       putStr ("\n\t Minas do jogador: "++show(minasJogador))
                       putStr ("\n\t Minas do campo: "++show(minasMapa))
                       putStr ("\n\t Celulas vazias: "++show(qtdCelulasVazias))
                       putStrLn "\n--------------------------------------------------------------------------\n"

{-
  Simula um 'for' da linguagem imperativa e faz uma repetição para imprimir 
  a matriz do campo minado no console.
-}
forLoopPrintMapa :: (Int, Int, Int, Int, Int, [Celula]) -> IO()
forLoopPrintMapa(i, tamanho, lins, cols, opcao, ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms)) =
      do
        if(opcao == 0)                                      -- Imprime os titulos das colunas
          then do
            if(i == 0) 
              then do
                 putStr ("\t ")
                 putStr ("    | A | ")
                 forLoopPrintMapa((i + 1), tamanho, lins, cols, opcao, ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms))
              else
                if(i < cols)
                  then do
                    putStr ([(chr(ord ('A') + i))]++" | ")    -- Coloca o caractere corespondente a coluna
                    forLoopPrintMapa((i + 1), tamanho, lins, cols, opcao, ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms))
                else do                                       -- Terminou de escrever o titulo das linhas, volta no começo com a lista cheia pra escrever as colunas
                    putStrLn ("\n")
                    forLoopPrintMapa(0, tamanho, lins, cols, 1, ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms))
        else do
          if(opcao == 1)                                      -- Imprime a linha e depois vai pra opcao 2 pra imprimir os elementos de cada linha
           then do 
             putStr ("\t " ++ show(idLinha) ++ " - | ")
             forLoopPrintMapa(0, tamanho, lins, cols, 2, ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms))
          else do
            if(opcao == 2)                                    -- Imprime elementos da linha
              then do
                if(length(ms) > 0)                            -- Se o tamanho do tail da lista for maior que zero significa que tem mais de um elemento
                  then do
                    if(i < cols)
                      then do
                        putStr(escrito ++ " | ")
                        forLoopPrintMapa((i + 1), tamanho, lins, cols, 2, ms)
                    else do
                      putStr("\n")
                      forLoopPrintMapa(0, tamanho, lins, cols, 1, ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms))
                else do                                        -- Chegou no último elemento da lista
                  putStr(escrito ++ " | ")                     -- Escrito do ultimo elemento
                  putStrLn("\n\n\t Impressao do mapa concluida com sucesso!!!")
            else do
              print("Outras opcoes alem da 2")


{-
    Recebe uma lista dos indices do vetor que armazena o campo minado
    , em seguida é embaralhada a lista e a cabeça dessa lista é inserida
    em uma lista vazia e assim ao final retorna a lista de sorteados com
    as posições das minas
-}
listaAleatoria:: [Int] -> Int -> [Int] -> IO [Int]
listaAleatoria (x:xs) qtd listaSorteada = 
                    do
                      if(length(listaSorteada) < qtd)
                        then do
                          let listEmb = (listaEmbaralhada (x:xs))
                          let caudaListEmb = (tail(listEmb))
                          let cabecaListEmb = (head(listEmb))
                          listaAleatoria caudaListEmb qtd (cabecaListEmb:listaSorteada)
                        else do
                          return(sort(listaSorteada))--return ()

posicionarMinas :: (Int, Int, Int, Int, [Int], [Celula], [Celula]) -> IO [Celula]  -- linha, coluna, mapa
posicionarMinas(i, tamanho, opcao, qtdMinas, sorteados, ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms), mapaAtualizado) = 
                   do
                     if(opcao == 0)                     -- Sorteia as posições
                      then do
                        putStrLn "posicionar Minas:"
                        sorteados <- (listaAleatoria  [0..(tamanho - 1)] qtdMinas [])
                        --print("Sorteados("++show(length(sorteados))++"): "++show(sorteados))
                        posicionarMinas(i, tamanho, 1, qtdMinas, sorteados, ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms), mapaAtualizado)
                      else do                           -- Acerta o campo minado atribuindo as posicoes sorteadas na opcao 1
                        --putStrLn("\n i = "++show(i)++"\n")
                        --putStrLn("\nTam ms: "++ show(length(ms))++" -- Estrutura: "++show(ms)++"\n")
                        if(i < tamanho) -- Adiciona
                          then do
                            --putStrLn("\n sorteados tamanho: "++show(length(sorteados))++"\n")
                            if(length(sorteados) /= 0) -- Passa pelas posicoes sorteadas
                              then do
                                 let cabecaLista = head sorteados
                                 let caudaLista = tail sorteados
                                 --putStrLn(show(i)++" i--head "  ++ show(cabecaLista))
                                 --putStrLn(show(i)++" i--cauda " ++ show(caudaLista) ++ " tam: " ++ show(length(caudaLista)))
                                 --putStrLn("Tam mapaAtualizado: "++ show(length(mapaAtualizado))++" -- Estrutura: "++show(mapaAtualizado))
                                 {-
                                    O if abaixo trata o erro na recursao finalizando e devolvendo o mapa alterado:
                                    *** Exception: codigo.hs:(307,1)-(336,50): Non-exhaustive patterns in function posicionarMinas
                                    ocorrido quando Sorteados(8): [1,4,7,12,13,17,18,19]
                                    i = 19
                                    19 i--head 19
                                    19 i--cauda [] tam: 0

                                 -}
                                 if((i == (tamanho - 1)) && (length(caudaLista) == 0) && (i == cabecaLista))    -- Tratando erro
                                  then do
                                    --putStrLn("Excessao")
                                    return((mapaAtualizado ++ [(Celula escrito idLinha idColuna fechado True marcacaoMinaJogador vizinho)]))
                                  else do
                                    if(i == cabecaLista)
                                      then do         -- Só remove da lista de sorteados se o elemento estiver na cabeça da lista, em seguida é passado a cauda
                                        posicionarMinas((i + 1), tamanho, 1, qtdMinas, caudaLista, ms, (mapaAtualizado ++ [(Celula escrito idLinha idColuna fechado True marcacaoMinaJogador vizinho)]))
                                      else do
                                        posicionarMinas((i + 1), tamanho, 1, qtdMinas, sorteados, ms, (mapaAtualizado ++ [(Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho)]))
                              else do                 -- Adiciona o restante das celulas, e retorna
                                let aux2 = ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms)
                                let aux = (mapaAtualizado ++ aux2)
                                return(aux)           
                          else do
                            putStrLn("Retorno")
                            return(mapaAtualizado)



-- Realiza o calculo para cada célula da quantidade de vizinhos que possuem minas em volta
calculaVizinhos:: (Int, Int, Int, Int, [Celula], [Celula], [Celula]) -> IO [Celula]
calculaVizinhos(i, tamanho, lins, cols, ((Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho):ms), mapaVerificacao, mapaAtualizado) =
                  do
                  if(i < tamanho)
                    then do
                      aux <- (retornaVizinhos(0, (Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho), lins, cols, mapaVerificacao, 0))
                      putStrLn("i: "++ show(i) ++ " - ms - "++show(length(ms))++"- ["++show(idLinha)++" , "++show(idColuna)++"]: "++ show(aux))
                      if(i /= (tamanho - 1))
                        then do
                          calculaVizinhos((i+1), tamanho, lins, cols, ms, mapaVerificacao, (mapaAtualizado ++ [(Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador aux)]))
                        else do             -- Tratamento de erro de recursão do ultimo elemento
                          return(mapaAtualizado ++ [(Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador aux)])
                    else do
                      print("FFFFF")
                      return(mapaAtualizado)

-- Função auxiliar ao calculaVizinho, este retorna a soma dos vizinhos de uma determinada célula
retornaVizinhos:: (Int, Celula, Int, Int, [Celula], Int) -> IO Int
retornaVizinhos(opcao, cell, lins, cols, mapa, contador) = 
                do
                let c = (obterIDColuna cell)
                let l = (obterIDLinha cell)
                if(opcao == 0) -- Linha maior => Verifica para baixo
                  then do
                    if((l + 1) <= (lins - 1)) -- Dentro dos limites
                      then do
                        let aux = (retornaCelulaPelaMatriz((l + 1), c, mapa))
                        if((obterEhMina aux) == True)
                          then do
                            print("Tem mina em baixo: "++ show(aux))
                            retornaVizinhos(1, cell, lins, cols, mapa, (contador+1))
                          else do
                            retornaVizinhos(1, cell, lins, cols, mapa, contador)
                      else do
                        retornaVizinhos(1, cell, lins, cols, mapa, contador)
                  else do
                    if(opcao == 1) -- Linha menor => Verifica para cima
                      then do
                        if((l - 1) >= 0) -- Dentro dos limites
                          then do
                            let aux = (retornaCelulaPelaMatriz((l - 1), c, mapa))
                            if((obterEhMina aux) == True)
                              then do
                                print("Tem mina em cima: "++ show(aux))
                                retornaVizinhos(2, cell, lins, cols, mapa, (contador+1))
                              else do
                                retornaVizinhos(2, cell, lins, cols, mapa, contador)
                          else do
                            retornaVizinhos(2, cell, lins, cols, mapa, contador)
                      else do
                        if(opcao == 2) -- Coluna maior => Verifica para direita
                          then do
                            if((c + 1) <= (cols - 1)) -- Dentro dos limites
                              then do
                                let aux = (retornaCelulaPelaMatriz(l, (c + 1), mapa))
                                if((obterEhMina aux) == True)
                                  then do
                                    print("Tem mina na direita: "++ show(aux))
                                    retornaVizinhos(3, cell, lins, cols, mapa, (contador+1))
                                  else do
                                    retornaVizinhos(3, cell, lins, cols, mapa, contador)
                              else do
                                retornaVizinhos(3, cell, lins, cols, mapa, contador)

                          else do
                            if(opcao == 3) -- Coluna menor => Verifica para esquerda
                              then do
                                if((c - 1) >= 0) -- Dentro dos limites
                                  then do
                                    let aux = (retornaCelulaPelaMatriz(l, (c - 1), mapa))
                                    if((obterEhMina aux) == True)
                                      then do
                                        print("Tem mina na esquerda: "++ show(aux))
                                        retornaVizinhos(4, cell, lins, cols, mapa, (contador+1))
                                      else do
                                        retornaVizinhos(4, cell, lins, cols, mapa, contador)
                                  else do
                                    retornaVizinhos(4, cell, lins, cols, mapa, contador)
                              else do
                                return(contador)

                



--i,lin, col,

retornaCelulaPelaMatriz:: (Int, Int, [Celula]) -> Celula
retornaCelulaPelaMatriz (i, j, lis) = 
              do
              let col = (obterIDColuna (last (lis)) + 1)
              let indexVetor = (j + i * col)
              obterCelula(indexVetor, lis)

obterCelula::(Int, [Celula]) -> Celula
obterCelula(i,lis) = (lis!!i)

soma:: Int ->IO Int
soma x = do 
         putStr("")
         return(x + 1)
          

forLoop :: Int -> Int -> Int -> Int
forLoop i tamanho valor =
      if i < tamanho
           then forLoop (i + 1) tamanho (3 + valor)
           else valor

-- Trata a quantidade de minas informada pelo jogador
tratarMinas:: Int -> Int -> IO Int
tratarMinas x y = do
                  let maxMinas = fromIntegral((x * y) `div` 2)  -- funcao temporaria que pega a parte inteira e controla o maximo de minas que pode ter no cenário
                  putStr("Digite o numero de minas entre (1 - " ++ show(maxMinas) ++ "): ")
                  minas <- getLine
                  let auxMinas = (read(minas):: Int)
                  if(auxMinas > maxMinas) then do
                        return(maxMinas)
                  else if(auxMinas < 1) then do
                        return(1)
                  else return(auxMinas)
                  
{-
  FUNÇÕES PARA OBTER OS ATRIBUTOS INTERNOS A CELULA DO MAPA
-}

obterEscrito :: Celula -> String
obterEscrito (Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho) = escrito

obterIDLinha :: Celula -> Int
obterIDLinha (Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho) = idLinha

obterIDColuna :: Celula -> Int
obterIDColuna (Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho) = idColuna

obterFechado :: Celula -> Bool
obterFechado (Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho) = fechado

obterEhMina :: Celula -> Bool
obterEhMina (Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho) = mina

obterMarcacaoMinaJogador :: Celula -> Bool
obterMarcacaoMinaJogador (Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho) = marcacaoMinaJogador

obterVizinho :: Celula -> Int
obterVizinho (Celula escrito idLinha idColuna fechado mina marcacaoMinaJogador vizinho) = vizinho