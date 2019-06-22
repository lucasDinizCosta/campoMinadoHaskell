module Matriz where

import Data.Array
import Data.List
import Control.Monad

--geraMatriz :: Int -> Int -> Array
--geraMatriz 0 0 = array((0,0),(0,0))
--geraMatriz linhas colunas = array((0,0),(linhas, colunas))
type IdLinha = Int
type IdColuna = Int
type Fechado = Bool		-- Contrasta os vizinhos e não pode mais ser visitado
type Mina = Bool        -- A celula possui mina
type Estado = Bool      -- False => não tem mina marcada pelo jogador, True => tem minha marcada pelo jogador
type Vizinho = Int      -- Quantidade de vizinhos com minas
data Celula = Celula IdLinha IdColuna Fechado Mina Estado Vizinho

--	CRIAR UMA MATRIZ DE CELULAS


matTeste = array ((1,1),(2,3)) [((1,1),4), ((1,2),0), ((1,3),3),((2,1),5), ((2,2),1), ((2,3),4)]
mat = array ((1,1),(2,3)) [((1,1),4), ((1,2),0), ((1,3),3),((2,1),5), ((2,2),1), ((2,3),4)]
--teste = array ((1,1),(2,2))
linhas = 2
colunas = 2
--main :: _ -> Array(a,a) b
geraMatrizZeros = do
		print (linhas)
		putStr "Digite o numero de linhas: "
		linhas <- getLine
		putStr "Digite o numero de colunas: "
		colunas <- getLine
		putStrLn (linhas++" "++colunas++"\n")
		return( array ((0,0),(((read linhas :: Int) - 1), ((read colunas :: Int) - 1))) [((i,j), 0) | (i, j) <- range ((0,0),(((read linhas :: Int) - 1), ((read colunas :: Int) - 1)))])

geraMatriz = do
		print (linhas)
		putStr "Digite o numero de linhas: "
		linhas <- getLine
		putStr "Digite o numero de colunas: "
		colunas <- getLine
		putStrLn (linhas ++ " " ++ colunas ++ "\n")


		--return( array ((0,0),(((read linhas :: Int) - 1), ((read colunas :: Int) - 1))) [((i,j),  valor) | (i, j) <- range ((0,0),(((read linhas :: Int) - 1), ((read colunas :: Int) - 1)))])

--removeIndiceLista :: [a] -> [a]
removeIndiceLista n lista = delete (lista!!n) lista
