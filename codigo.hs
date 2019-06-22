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
import System.IO.Unsafe
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

numeroMinas = 8
linhas = 4
colunas = 4
listaTeste = [1..10]

--removeIndiceLista :: Int -> [Int] -> [Int]
removeIndiceLista n x = (delete (x!!n) x)

{-
    FUNÇÕES AUXILIARES DE EMBARALHAMENTO E ALEATOREIDADE
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

getElementoAleatorioLista:: [a] -> a
getElementoAleatorioLista lis = (listaEmbaralhada(lis))!!0

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
    putStrLn (linhas ++ " " ++ colunas ++ "\n")
