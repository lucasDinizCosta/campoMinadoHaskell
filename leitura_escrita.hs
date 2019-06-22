import System.IO

{-

	Modos:

	WriteMode => Modo para escrita no arquivo (O Conteúdo é sobreescrito)
	ReadMode => Modo para a leitura do arquivo
	ReadWriteMode => Permissão para a Leitura e Escrita no arquivo
	AppendMode => Adicionar contéudo no final do arquivo
-}

--	PRIMEIRO MÉTODO: USANDO O MANIPULADOR DE ARQUIVOS

escrever_arquivo :: IO ()
escrever_arquivo = do
				arq <- openFile "teste.txt" AppendMode
				hPutStr arq "Escrita no final do arquivo\n"
				--hPutStr arq "Escrevendo novamente\n"
				putStrLn "Escrita realizada com sucesso!"
				hFlush arq   -- Liberando o buffer
				hClose arq   -- Fechando o arquivo

ler_arquivo :: IO ()
ler_arquivo = do
				arq <- openFile "teste.txt" ReadMode
				conteudo <- hGetContents arq
				putStrLn conteudo
				hClose arq

--	SEGUNDO MÉTODO: MANÍPULANDO OS ARQUIVOS DIRETAMENTE

escrever :: IO ()
escrever = do
			writeFile "teste.txt" "Aprendendo Haskell"
			putStrLn "Escrita realizada com sucesso!"

ler :: IO ()
ler = do
		conteudo <- readFile "teste.txt"
		putStrLn conteudo

anexar :: IO ()
anexar = do
		appendFile "teste.txt" "\nHaskell eh legal"
		putStrLn "Conteudo anexado com sucesso!"