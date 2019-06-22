import System.IO
import System.Random
import System.IO.Unsafe

{-

	MUDANCA DO VALOR DE UMA FUNÇÃO POR MEIO DA
	LEITURA E ESCRITA DE UM ARQUIVO DE TEXTO
-}

linhas = 1
lista = [1,2,3]

muda_valor :: Int -> Int
muda_valor x = x + 1

main :: IO ()
main = do
		putStrLn "\tIncrementado a \'variavel\' linhas: "
		--	Lê o valor presente no arquivo de texto e armazena em 'linhas'
		putStrLn "Lendo o valor do arquivo de texto."
		arq <- openFile "teste.txt" ReadMode
		linhas <- hGetLine arq
		putStrLn linhas
		hClose arq
		putStrLn ("Leitura realizada com sucesso. linhas: " ++ linhas)
		--putStrLn (show(muda_valor(read linhas :: Int)))
		
		--	Abre o arquivo e insere o valor lido incrementado
		arqEscrita <- openFile "teste.txt" WriteMode

		--	Converte o texto lido para Int, incrementa em 1 e converte pra String de novo e salva no arquivo de texto
		hPutStrLn arqEscrita (show(muda_valor(read linhas :: Int)))
		putStrLn "Escrita realizada com sucesso!\n"
		hFlush arqEscrita   -- Liberando o buffer
		hClose arqEscrita   -- Fechando o arquivo

incrementaLista :: [Int] -> [Int]
incrementaLista lis = lis ++ [(last(lis) + 1)]

main2 :: IO ()
main2 = do
		putStrLn "\tIncrementado a lista: "
		--	Lê o valor presente no arquivo de texto e armazena em 'linhas'
		putStrLn "Lendo o valor do arquivo de texto."
		arq <- openFile "teste2.txt" ReadMode
		lista <- hGetLine arq
		putStrLn lista
		hClose arq
		putStrLn ("Leitura realizada com sucesso. lista: " ++ (show lista))
		print (read lista :: [Int])	-- Mudando de String pra lista
		--putStrLn (show(muda_valor(read linhas :: Int)))
		
		--	Abre o arquivo e insere o valor lido incrementado
		arqEscrita <- openFile "teste2.txt" WriteMode

		--	Converte o texto lido para Int, incrementa em 1 e converte pra String de novo e salva no arquivo de texto
		hPutStrLn arqEscrita (show(incrementaLista(read lista :: [Int])))
		putStrLn "Escrita realizada com sucesso!\n"
		hFlush arqEscrita   -- Liberando o buffer
		hClose arqEscrita   -- Fechando o arquivo

numAleatorio:: Int -> Int -> IO(Int)
numAleatorio ini fim = randomRIO (ini, fim)

--numAleatorioV2:: Int -> Int -> Int
--numAleatorioV2 ini fim = unsafePerformIO(numAleatorio(ini, fim))


shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

posicaoListaAleatoria:: [Int] -> IO Int
posicaoListaAleatoria xs = getStdRandom (randomR (0, length xs - 1))

-- Retira o IO a para o tipo padrão do dado
transform::IO a -> a
transform x = unsafePerformIO x

listaEmbaralhada:: [a] -> [a]
listaEmbaralhada lis = transform(shuffle lis)

getElementoAleatorioLista:: [a] -> a
getElementoAleatorioLista lis = (listaEmbaralhada(lis))!!0

{-elementoListaAleatoria:: IO [Int] -> Int
elementoListaAleatoria xs = do
							aux <- unsafePerformIO $ getStdRandom (randomR (0, length xs - 1))
							return(xs!!aux)
-}

--numAleatorio:: Int -> Int -> Int -> Int
--numAleatorio ini fim qtd = do--take qtd $ randomRs (ini,fim) (mkStdGen 3) :: [Int]  
							--g <- newStdGen
							--return(fst(randomR(ini,fim) g))
						--teste <- randomRIO (ini::Int,fim::Int) >>= (\x -> return(x))
						--return(teste)
					 --(randomRIO (ini::Int,fim::Int) >>= (\x -> return(x)))

--randomList :: Int -> Int -> Int -> [Int]
--randomList ini fim 0 = []
--randomList ini fim n = do
 -- randomRIO (ini::Int,fim::Int) >>= (\x -> [x]++(randomList ini fim (n-1)))
--geraListaSorteada :: Int -> Int -> Int -> [Int]
--geraListaSorteada ini fim 0 = [] 
--geraListaSorteada ini fim qtd = do
								--aleat <- randomRIO (ini::Int, fim::Int)
								--[read aleat :: Int]++geraListaSorteada ini fim qtd--geraListaSorteada ini fim (qtd - 1)
			--num <- randomRIO (1::Int, 50) -- Sorteando um número de 1 a 50
