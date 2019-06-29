-----------------------------------------------------------------------------------------------------------------------------
-- 										Trabalho de linguagem de programação:												-
--																															-
-- 	Tema: Implementação do campo minado em Haskell																			-
--																															-
--	Nome: Lucas Diniz da Costa		Matricula: 201465524AC																	-
-- 																															-
-----------------------------------------------------------------------------------------------------------------------------

module CampoMinado where

import Graphics.UI.Gtk
import Data.Array
import Data.List
import System.IO
import System.Random
import System.Process           -- Para poder ativar a limpeza do console
import System.IO.Unsafe
import Data.Char                -- Algumas funções interessantes de caracteres
{-  
  Trabalhando com matrizes:

  A primeira tupla define o primeiro elemento da matriz e
  a segunda tupla define o elemento final
  matrix 2x2

  -> uma célula tem 4 vizinhos;
  -> Jogada:
    posicao | posicao a ser aberta                 | Exemplo: A1
    +posicao| posicao marcada como mina            | Exemplo: +D2 =>
    -posicao| desmarcar posicao marcada como minaa | Exemplo: -D2

  -> Posicionamento das minas é sorteado
  -> A cada iteração deve ser impressa a matriz no campo
  -> O jogador digita o número de minas. Por exemplo:
     Se o mapa é 4x4, o numero de bombas deve ser entre 1 e 8
     caso o usuário digitar maior, deve ser colocado como o máximo,
     ou seja, 8 minas

  *** Ideia de sorteio da posicao das bombas:

  será criada uma lista com os indices da matriz transformada em uma lista.
  Exemplo: matrix 4x4, lista com 16 indices [0..16]

  Ao sortear uma posição, o elemento deve ser removido da lista.
  Exemplo: sorteio 8, logo a lista terá 15 elementos [0..7,9,10..16]
  e o valor dele será convertido para o indice na matriz.
  lista[8] = lista [] = matriz[linha][coluna]
  linha = indice / Quantidade de elementos por linha
  coluna = indice % Quantidade de elementos por linha
  import Data.List
  delete ([1..16]!!0) [1..16] => [2,3,4..16]  ==> Removendo o elemento da posição zero

-}
--get_matrix = array ((1,1), (2,2)) [((1,1),'A'),((1,2),'B'),((2,1),'C'),((2,2),'D')]

minas = 8
linhas = 4
colunas = 4
listaTeste = [1..10]

--removeIndiceLista :: Int -> [Int] -> [Int]
removeIndiceLista n x = (delete (x!!n) x)

-- definição dos tipos dos dados

--type CampoMinado = [Celula]
type IdLinha = Int
type IdColuna = Int
type Fechado = Bool     -- Contrasta os vizinhos e não pode mais ser visitado
type Mina = Bool        -- A celula possui mina
type Estado = Bool      -- False => não tem mina marcada pelo jogador, True => tem minha marcada pelo jogador
type Vizinho = Int      -- Quantidade de vizinhos com minas
type Escrito = String   -- Escrito da celula que mostra o estado dela, por padrão é '*'
data Celula = Celula Escrito IdLinha IdColuna Fechado Mina Estado Vizinho
                              deriving(Show, Eq)

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

--getElementoAleatorioLista:: [a] -> a
--getElementoAleatorioLista lis = (listaEmbaralhada(lis))!!0


{-
    FUNÇÕES DE TRABALHO COM INTERFACE
-}
muda_valor :: Int -> Int
muda_valor x = x + 1


teste :: IO ()
teste = do
    putStrLn "\tIncrementado a \'variavel\' linhas: "
    --  Lê o valor presente no arquivo de texto e armazena em 'linhas'
    putStrLn "Lendo o valor do arquivo de texto."
    arq <- openFile "teste.txt" ReadMode
    linhas <- hGetLine arq
    putStrLn linhas
    hClose arq
    putStrLn ("Leitura realizada com sucesso. linhas: " ++ linhas)
    --putStrLn (show(muda_valor(read linhas :: Int)))
    
    --  Abre o arquivo e insere o valor lido incrementado
    arqEscrita <- openFile "teste.txt" WriteMode

    --  Converte o texto lido para Int, incrementa em 1 e converte pra String de novo e salva no arquivo de texto
    hPutStrLn arqEscrita (show(muda_valor(read linhas :: Int)))
    putStrLn "Escrita realizada com sucesso!\n"
    hFlush arqEscrita   -- Liberando o buffer
    hClose arqEscrita   -- Fechando o arquivo

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
               windowTitle := "Campo minado", containerBorderWidth := 20]

  window `on` focus $ \dirtype -> putStrLn "Janela criada!" >> return False

  --removeIndiceLista :: Int -> [a] -> [a]
  --removeIndiceLista n lista = delete (lista!!n) lista

  -- Cria um agrupamento de botões
  hbuttonbox <- hButtonBoxNew

  set window [ containerChild := hbuttonbox ]

  button1 <- buttonNewWithLabel "One"
  button2 <- buttonNewWithLabel "Two"
  button3 <- buttonNewWithLabel "Three"

  onClicked button1 (print(linhas))  -- Atribuindo função a um botão
  onClicked button2 (print(listaTeste))  -- Atribuindo função a um botão
  onClicked button1 $ do
    geraMatriz
  onClicked button3 $ do
              teste
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

geraMatriz = do
    print (linhas)
    putStr "Digite o numero de linhas: "
    linhas <- getLine
    putStr "Digite o numero de colunas: "
    colunas <- getLine
    putStrLn (linhas++" "++colunas++"\n")
    --limiteLinha <- ((read linhas :: Int) - 1)
    --limiteColuna <- ((read colunas :: Int) - 1)
    --return( array ((0,0),(limiteLinha, limiteColuna)) [((i,j), 0) | (i, j) <- range ((0,0), (limiteLinha, limiteColuna))])

-- função que prepara o início do jogo
--prepararJogo :: Jogadores -> IO Jogadores
prepararJogo:: IO()
prepararJogo = do
               putStr "Digite o numero de linhas: "
               linhas <- getLine
               putStr "Digite o numero de colunas: "
               colunas <- getLine
               putStrLn (linhas ++ " " ++ colunas ++ "\n")
               minas <- (tratarMinas(read (linhas):: Int) (read(colunas):: Int))
               putStrLn (linhas ++ " " ++ colunas ++ " " ++ (show(minas)) ++ "\n")
               campoMinado <- montarMapa (read (linhas):: Int) (read (colunas):: Int) minas
               --printMapa (read (linhas):: Int) (read (colunas):: Int) (read (minas):: Int) campoMinado
               putStr ""   -- Se tirar causa erro de identação
               

montarMapa:: Int -> Int -> Int -> IO [Celula]
montarMapa x y z = do
                   putStrLn "montar Mapa:"
                   let mapa = [(Celula "*" r c False False False 0) | r <- [0..(x - 1)] , c <- [0..(y - 1)]]--let mapa = [(Celula "*" x y False False False 0)];
                   --print(show(length(mapa))++ " --  elementos na matriz.")
                   mapa <- posicionarMinas(0, (x*y), 0, z, [], mapa, [])
                   --mapa <- posicionarMinas(0, (x*y), 0, z, [1,4,7,12,13,17,18,19], mapa, [])
                   printMapa x y z mapa
                   return(mapa)   -- Retornando uma lista de células

printMapa:: Int -> Int -> Int -> [Celula] -> IO()
printMapa x y z mapa = do
                       system "clear"     -- no windows eh 'system "cls"'
                       print(show(length(mapa))++ " --  elementos na matriz.")
                       putStrLn "Imprimindo mapa:"
                       putStrLn "\n--------------------------------------------------------------------------\n"
                       forLoopPrintMapa(0, (x*y), x, y, 0, mapa)
                       putStrLn "\n--------------------------------------------------------------------------\n"

{-
  Simula um 'for' da linguagem imperativa e faz uma repetição para imprimir 
  a matriz do campo minado no console.
-}
forLoopPrintMapa :: (Int, Int, Int, Int, Int, [Celula]) -> IO()
forLoopPrintMapa(i, tamanho, lins, cols, opcao, ((Celula escrito idLinha idColuna fechado mina estado vizinho):ms)) =
      do
        if(opcao == 0)                                      -- Imprime os titulos das colunas
          then do
            if(i == 0) 
              then do
                 putStr ("\t ")
                 putStr ("    | A | ")
                 forLoopPrintMapa((i + 1), tamanho, lins, cols, opcao, ((Celula escrito idLinha idColuna fechado mina estado vizinho):ms))
              else
                if(i < cols)
                  then do
                    putStr ([(chr(ord ('A') + i))]++" | ")    -- Coloca o caractere corespondente a coluna
                    forLoopPrintMapa((i + 1), tamanho, lins, cols, opcao, ((Celula escrito idLinha idColuna fechado mina estado vizinho):ms))
                else do                                       -- Terminou de escrever o titulo das linhas, volta no começo com a lista cheia pra escrever as colunas
                    putStrLn ("\n")
                    forLoopPrintMapa(0, tamanho, lins, cols, 1, ((Celula escrito idLinha idColuna fechado mina estado vizinho):ms))
        else do
          if(opcao == 1)                                      -- Imprime a linha e depois vai pra opcao 2 pra imprimir os elementos de cada linha
           then do 
             putStr ("\t " ++ show(idLinha) ++ " - | ")
             forLoopPrintMapa(0, tamanho, lins, cols, 2, ((Celula escrito idLinha idColuna fechado mina estado vizinho):ms))
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
                      forLoopPrintMapa(0, tamanho, lins, cols, 1, ((Celula escrito idLinha idColuna fechado mina estado vizinho):ms))
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
posicionarMinas(i, tamanho, opcao, qtdMinas, sorteados, ((Celula escrito idLinha idColuna fechado mina estado vizinho):ms), mapaAtualizado) = 
                   do
                     if(opcao == 0)                     -- Sorteia as posições
                      then do
                        putStrLn "posicionar Minas:"
                        sorteados <- (listaAleatoria  [0..(tamanho - 1)] qtdMinas [])
                        --print("Sorteados("++show(length(sorteados))++"): "++show(sorteados))
                        posicionarMinas(i, tamanho, 1, qtdMinas, sorteados, ((Celula escrito idLinha idColuna fechado mina estado vizinho):ms), mapaAtualizado)
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
                                    --posicionarMinas((i + 1), tamanho, 1, qtdMinas, caudaLista, ms, (mapaAtualizado ++ [(Celula escrito idLinha idColuna fechado True estado vizinho)]))
                                    return((mapaAtualizado ++ [(Celula escrito idLinha idColuna fechado True estado vizinho)]))
                                  else do
                                    if(i == cabecaLista)
                                      then do         -- Só remove da lista de sorteados se o elemento estiver na cabeça da lista, em seguida é passado a cauda
                                        posicionarMinas((i + 1), tamanho, 1, qtdMinas, caudaLista, ms, (mapaAtualizado ++ [(Celula escrito idLinha idColuna fechado True estado vizinho)]))
                                      else do
                                        posicionarMinas((i + 1), tamanho, 1, qtdMinas, sorteados, ms, (mapaAtualizado ++ [(Celula escrito idLinha idColuna fechado mina estado vizinho)]))
                              else do                 -- Adiciona o restante das celulas, e retorna
                                let aux2 = ((Celula escrito idLinha idColuna fechado mina estado vizinho):ms)
                                let aux = (mapaAtualizado ++ aux2)
                                return(aux)           
                          else do
                            putStrLn("Retorno")
                            return(mapaAtualizado)

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
                  

--minas = (read(minas):: Int)
                  