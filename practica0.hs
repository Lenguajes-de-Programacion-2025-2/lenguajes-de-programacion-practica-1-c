module Practica0 where


{-- Recursion y recursion de Cola --}

--Funcion buscar : Dada una lista de Enteros y elemento , Regresa verdadero en caso de que el elemento se encuentre en la lista
--En otro caso regresa False 

buscar::[Int]->Int->Bool
buscar [] _ = False
buscar (x:xs) e = if x == e then True else buscar xs e


--Funcion sumar_lista : Dada una Lista de Entero , regresa la suma de sus elementos
--Implementala con recursion de Cola
sumar_lista::[Int]->Int
sumar_lista = sumar_listaAux 0


--Implementa una funcion recursiva de forma "ordinaria" y despues implementala con recursion de cola
--Usa los comandos vistos en clase para comparar tiempo y memoria usados y dado el resultado describe que sucedio
--Y porque crees que haya sido asi
-- :s +t (en el ghci  para ver la estadisticas )
sumar_listaAux :: Int -> [Int] -> Int
sumar_listaAux acc [] = acc
sumar_listaAux acc (x:xs) = sumar_listaAux (acc + x) xs


--
{--funciones--}

--Funcion filter toma un predicado (funcion que regresa booleano) y filtra los elementos la lista de entrada  dada la condicion
filterB:: (a -> Bool) -> [a] -> [a]
filterB _ [] = []
filterB p (x:xs) = if p x then x : filterB p xs else filterB p xs

--Implementa una funcion llamada mapear que reciba como argumento una funcion y aplique esta funcion a una lista
mapear:: (a->b) -> [a] -> [b]
mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

--Decima extra : .2
--Forma comprehension
mapear_:: (a->b) -> [a] -> [b]
mapear_ f list = [f x | x <- list]



{--Tipos,clases y Estructuras de Datos --}

--Arbol 
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)



--Dada la definicion de arbol binario has una funcion que haga un recorrido pre order
preorder:: Tree a -> [a]
preorder Empty =  []
preorder (Node root left right) =  [root] ++  preorder left ++ preorder right

--Hacer una funcion que calcule la altura del arbol ,regresa verdadero en caso de encontrar el eelemento en el arbol
buscar_tree:: Eq a => Tree a -> a -> Bool
buscar_tree Empty e  = False
buscar_tree (Node a b c) e = (e == a) || (buscar_tree b e || buscar_tree c e)

alturaAB :: Tree a -> Int
alturaAB Empty = 0
alturaAB (Node a Empty Empty) = 0
alturaAB (Node a b c) = 1 + max (alturaAB b) (alturaAB c)

--Punto Extra:  Implementa  una funcion que cuente la cantidad de hojas del arbol 
hojas:: Tree a -> Int
hojas Empty  = 0
hojas (Node _ Empty Empty) = 1
hojas (Node a b c) = hojas b + hojas c


--Definicion de Grafica 

type Vertex = Int
type Graph = [(Vertex, [Vertex])]

vecinos :: Graph -> Vertex -> [Vertex]
vecinos [] _ = []
vecinos ((v, ns):xs) x
    | v == x    = ns
    | otherwise = vecinos xs x

dfs :: Graph -> Vertex -> [Vertex] -> [Vertex]
dfs graph v visited
    | v `elem` visited = visited
    | otherwise = foldl (\acc n -> dfs graph n acc) (v : visited) (vecinos graph v)

--Dada la siguiente defincion de grafica , crea una funcion que verifique si la grafica es conexa 
--Tip: USA la funcion auxiliar dfs, (si quieres puedes usar otra de tu propio diseño)

isConnected :: Graph -> Bool   --Funcion a Implementar
isConnected [] = True
isConnected ((v, ns):xs) = obtenVertices ((v, ns):xs) == ordenarVertices (dfs ((v, ns):xs) 1 [])

-- función auxiliar que ordena los vertices obtenidos con dfs, para que se puedan comparar
-- con los vertices de la gráfica, ya que si dfs obtiene todos los vertices de la
-- grafica significa que cada uno de ellos está conectado con al menos otro.
ordenarVertices :: Ord a => [a] -> [a]
ordenarVertices [] = []
ordenarVertices (x:xs) = ordenarVertices [y | y <- xs, y <= x] ++ [x] ++ ordenarVertices [y | y <- xs, y > x]

-- Función auxiliar que obtiene los vertices de la gráfica, solamente verifica el primer
-- elemento de cada vertice, de esta forma obtiene cada vertice "ignorando" a quienes tiene
-- conectados
obtenVertices :: Graph -> [Vertex]
obtenVertices ((v, ns):xs) = map fst ((v, ns):xs)

--Ejemplos

connectedGraph :: Graph
connectedGraph = [(1, [2,3]), (2, [4]), (3, [4,5]), (4, [6]), (5, [6]), (6, [])]  --Debe Regresar True

disconnectedGraph :: Graph
disconnectedGraph = [(1, [2]), (2, [1]), (3, [4]), (4, [3])] --Debe regresar False 

graficaPrueba :: Graph
graficaPrueba = [(1, [2]), (2, [3,4]), (3, [4]), (4, [1])]

--La siguiente funcion verfiica que la grafica es un arbol 
--Tip : Recuerda que un arbol es una grafica conexa y sin ciclos
isTree :: Graph -> Bool
isTree []  = True
-- isTree ((v, ns):xs) = isConnected ((v, ns):xs) && not (tieneCiclos ((v, ns):xs) 1)

-- si de 5 llego a 4 y de los vecinos de 4 hay alguna forma de llegar a 5 entonces hay
-- un ciclo, necesito comparar eso para cada vertice hasta recorrer todos los vertices
-- por cada uno de los vertices revisa sus vecinos, si en los vecinos de este vertice 
-- aparece el vertice original entonces tiene un ciclo, si no aparece no lo tiene, pero revisa 
-- los otros hasta terminar la lista, en ese caso no tiene ciclos
--1 tiene de vecino a 2, se guarda, si 2 tiene de vecino a 
-- tieneCiclos :: Graph -> Vertex -> Bool
--tieneCiclos [] x = False
--tieneCiclos ((v, ns):xs) 0 = False
--tieneCiclos ((v, ns):xs) x = if head (ordenarVertices (vecinos ((v, ns):xs) x)) `elem` (vecinos ((v, ns):xs) (x-1))
  --  then True
  --  else if vecinos ((v, ns):xs) x == []
  --  then tieneCiclos ((v, ns):xs) (x+1)
  --  else if x > length ((v, ns):xs) then False
  --  else tieneCiclos ((v, ns):xs) (x+1)

arbolPrueba :: Graph
arbolPrueba = [(1, [2,3]), (2, [4]), (3, [5]), (4, []), (5, [6]), (6, [])]

verticeSum :: Int -> Vertex
verticeSum x = x+1

--La siguiente funcion regresa a suma de las hojas del arbol
leafSum:: Tree Int -> Int
leafSum Empty = 0
leafSum (Node a Empty Empty) = a
leafSum (Node a b c) = leafSum b + leafSum c