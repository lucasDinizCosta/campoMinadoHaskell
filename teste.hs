import Data.Array
--import Data.Matrix

--  Trabalhando com vetores:
--	Um array de 1 linha e 4 coluna
--	elems get_array => "ABCD" 			Retornando os elementos do vetor
--	get_array ! 2 => 'B' 				Retornando o elemento da posição 2
--	get_array ! 5 => ERRO POSICAO INVÁLIDA
--	bounds get_array => (1,4)			Retorna os limites do vetor
get_array = array (1, 4) [(1,'A'),(2,'B'),(3,'C'),(4,'D')]


{-	
	Trabalhando com matrizes:

	A primeira tupla define o primeiro elemento da matriz e
	a segunda tupla define o elemento final
	matrix 2x2
-}
get_matrix = array ((1,1), (2,2)) [((1,1),'A'),((1,2),'B'),((2,1),'C'),((2,2),'D')]


--	Implementação de uma pilha usando uma lista:
push :: [Int] -> Int -> [Int]
push pilha x = pilha ++ [x]

top :: [Int] -> Int
top [x] = x
top (x:xs) = top xs

pop :: [Int] -> [Int]
pop [] = error "Pilha vazia"
pop (x:xs) | (x == (top (x:xs))) = xs
		   | otherwise = x:(pop xs)

is_empty :: [Int] -> Bool
is_empty [] = True
is_empty _ = False

--  Definindo novos tipos de dados
-- Especifica o construtor do tipo de dado através do type
-- Aluno e Programador são construtores de valor
type Nome = String
type Linguagem = String
type Universidade = String
data Pessoa = Programador Nome Linguagem | Aluno Nome Universidade   -- Dois construtores: Tipo aluno e programador
                   deriving(Show)  -- Herda as funções de de show

programador = Programador "Lucas" "Haskell"
aluno = Aluno "Lucas" "UFJF"

is_programador :: Pessoa -> Bool
is_programador (Programador _ _) = True
is_programador _ = False

is_aluno :: Pessoa -> Bool
is_aluno (Aluno _ _) = True
is_aluno _ = False

-- Revisao

-- concatenar duas listas
concatena :: [a] -> [a] -> [a]
concatena [] y = y
concatena x [] = x
concatena (x:xs) y = x : concatena xs y

-- inverter uma lista
inv :: [a] -> [a]
inv [] = []
inv (x:xs) = (inv xs) ++ [x]

-- gerar uma lista infinita
gerar_lista :: Int -> [Int]
gerar_lista n = n : gerar_lista( n + 1 )

-- recebe uma lista de inteiros e retorna a 
-- soma de todos os elementos
soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma(xs)

-- minha função tail
my_tail :: [t] -> [t]
my_tail [] = []
my_tail (_:xs) = xs -- Não interessa o valor da cabeça, ou seja, x

type Pessoa2 = String
type Carro = String
type Idade = Int
type Registro = (Pessoa2, Carro, Idade)
type BD = [Registro]

f_bd :: BD
f_bd = [("Joao", "Camaro", 20), ("Maria", "Fusca", 30), ("Catarina", "Palio", 18)]

getNome :: Registro -> Pessoa2
getNome (n, _, _) = n

getCarros :: BD -> [String]
getCarros [] = []
getCarros ((_, carro, _):xs) = carro : getCarros xs

--	Implentação de árvore binária
{-
	Arvore de exemplo:
		 1
       /  \
      2    3
     / \   /
    4   5  6
   / \
 Nulo Nulo

 	Arvore binária de busca de exemplo:
		20
       /  \
     13    29
     / \   /
    4  18 22
   / \
 Nulo Nulo

-}

data ArvBin = Nulo | No Int ArvBin ArvBin -- Nó de inteiros com subarvore pra direita e esquerda
		deriving (Show)

arv :: ArvBin
arv = (No 1 
			(No 2 
				(No 4 Nulo Nulo) (No 5 Nulo Nulo))
	        (No 3 
	        	(No 6 Nulo Nulo) Nulo))

arvBinBusca :: ArvBin
arvBinBusca = (No 20 
				(No 13 
					(No 4 Nulo Nulo) (No 18 Nulo Nulo))
	        	(No 29 
	        		(No 22 Nulo Nulo) Nulo))

-- Percorrendo a arvore:

--	pre_ordem (esquerda, raiz, direita)
em_ordem :: ArvBin -> [Int]
em_ordem Nulo = []
em_ordem (No num esq dir) = (em_ordem esq) ++ [num] ++ (em_ordem dir)

--	pre_ordem (raiz, esquerda, direita)
pre_ordem :: ArvBin -> [Int]
pre_ordem Nulo = []
pre_ordem (No num esq dir) = [num] ++ (pre_ordem esq) ++ (pre_ordem dir) 

--	pos_ordem (esquerda, direita, raiz)
pos_ordem :: ArvBin -> [Int]
pos_ordem Nulo = []
pos_ordem (No num esq dir) = (pos_ordem esq) ++ (pos_ordem dir) ++ [num]


--	Inserção de elemento na arvore

insereArvBin ::  Int -> (ArvBin) -> (ArvBin)
insereArvBin num Nulo = No num Nulo Nulo
insereArvBin num (No valorNo esq dir)
		| num == valorNo = No num esq dir
		| num < valorNo = No valorNo (insereArvBin num esq) dir 
		| num > valorNo = No valorNo esq (insereArvBin num dir)

--	Busca elemento na arvore

buscaArvBin :: Int -> ArvBin -> Bool
buscaArvBin _ Nulo = False
buscaArvBin num (No valorNo esq dir) 
		| num == valorNo = True 
		| num < valorNo = buscaArvBin num esq  
		| num > valorNo = buscaArvBin num dir 

--	Menor elemento da árvore

menorArvBin :: (ArvBin) -> (Int)
menorArvBin Nulo = error "Arvore vazia"
menorArvBin (No valorNo Nulo dir) = valorNo
menorArvBin (No valorNo esq dir) = menorArvBin esq--if(num < valorNo) then menorArvBin esq else menorArvBin dir

--	Maior elemento da árvore

maiorArvBin :: (ArvBin) -> (Int)
maiorArvBin Nulo = error "Arvore vazia"
maiorArvBin (No valorNo esq Nulo) = valorNo
maiorArvBin (No valorNo esq dir) = maiorArvBin dir--if(num < valorNo) then menorArvBin esq else menorArvBin dir


--	3 JEITOS DIFERENTES DE FAZER A MESMA FUNÇÃO

if_par :: Int -> Bool
if_par n = if(mod n 2 == 0) then True else False

case_par :: Int -> Bool	
case_par n = case (mod n 2 == 0) of
				True -> True
				False -> False

guarda_par :: Int -> Bool
guarda_par n | (mod n 2 == 0 ) = True
			 | otherwise = False

--	UTILIZANDO WHERE
quad :: Int -> Int
quad n = quad_n
			where
				quad_n = n * n



--	Loops de repetição

--	Método 1

repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action

-- Método 2 - Imprimindo texto

printStringNTimes 0 = return ()
printStringNTimes n =
 do
  putStrLn "a string"
  printStringNTimes (n-1)

--	Implentação do quicksort

qsort [] = []
qsort (x:xs) = qsort esq_x ++ [x] ++ qsort dir_x
			where
				esq_x = [y | y <- xs, y <  x]
				dir_x = [y | y <- xs, y >= x]

--	funções de entrada
{-
	Funções de escrita no console:

	putChar 'A' => A 									-- Não quebra a linha
	putStr "Aprendendo Haskell"   => Aprendendo Haskell -- Não quebra a linha
	putStrLn "Aprendendo Haskell" => Aprendendo Haskell -- Quebra a linha
	print "Aprendendo Haskell"    => Aprendendo Haskell -- é da classe genérica show e possui quebra de linha

	Funções de leitura de dados escritos no console:

	getChar => Lê um caractere da entrada padrão(Teclado)
	getLine => Lê uma entrada e converte pra String
	read :: Read a => String -> a
	read => Lê uma String e converte pra um tipo especifico 'a'

	EXEMPLOS DE APLICAÇÃO:
-}



get_char :: IO ()
get_char = do
				putStr "Digite um caractere: "
				c <- getChar
				putStr "\nO caractere digitado foi: "
				putChar c
				putChar '\n'

somar_numeros :: IO Int
somar_numeros = do
					putStr "Digite o primeiro numero: "
					linha1 <- getLine
					putStr "Digite o segundo numero: "
					linha2 <- getLine
					return ((read linha1 :: Int) + (read linha2 :: Int))


