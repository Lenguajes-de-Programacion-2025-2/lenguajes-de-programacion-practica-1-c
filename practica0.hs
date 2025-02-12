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

arbolBinarioEjemplo :: Num a =>  Tree a
arbolBinarioEjemplo = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)

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
isTree graph = isConnected graph && not (hasCycle graph || any (tieneCiclos graph ) (obtenVertices graph))

-- Auxiliar para la funcion que detecta los ciclos
aux_filter :: Eq a => (a -> Bool) -> [a] -> Bool
aux_filter p xs = if (filterB p xs) == [] then False else True

hasCycle :: Graph -> Bool
hasCycle graph = aux_filter (detectCycle graph []) (obtenVertices graph)

-- Esta funcion detecta ciclos "directos" es decir detecta los ciclos que son del estilo
-- [(1, [2]), (2,[1])] o tambien [(1, [2, 3]), (2,[4]), (3, [1])] entre otros
detectCycle :: Graph -> [Vertex] -> Vertex -> Bool
detectCycle graph visited v
    | v `elem` visited = True
    | otherwise = aux_filter (detectCycle graph (v : visited)) (vecinos graph v)
    
-- Esta función auxiliar nos dice si alguno de los elementos de una lista pertenece a otra
-- Por ejemplo si tenemos la lista [1,2,3] y la [4,5,3] nos daría True ya que 3 aparece en ambas
-- La usamos en tieneCiclos para detectar si un vertice aparece en el dfs de los vecinos de otro vertice
-- la idea es que, si tenemos un vertice "x", si un vertice "y" aparece más de una vez en el dfs de los 
-- vecinos del vertice x, significa que hay un ciclo, ya que habría más de una forma de llegar
-- al mismo vertice "y" partiendo de un mismo vertice "x". 
algunoEnLista :: (Eq a) => [a] -> [a] -> Bool
algunoEnLista xs ys = any (`elem` ys) xs

-- Esta funcion encuentra ciclos que no son tan directos de ver, como ya lo explicamos en la
-- funcion anterior. Tiene muchjos if ya que debemos ser cuidadosos porque si no le damos
-- un limite a x se cicla, por eso el limite es la longitud de g, a lo más va a aplicar el 
-- algoritmo n veces, con n siendo el numero de vertices que existen
-- Y el otro if es porque si intentaramos el head de un vertice que no tiene vecinos nos
-- lanzaría un error ya que al ser la lista vacia no tiene cabeza, con esos
-- if ya no ocurre nada inesperado.
tieneCiclos :: Graph -> Vertex -> Bool
tieneCiclos g x = if x > length g
    then False
    else if (vecinos g x) == []
    then tieneCiclos g (x+1)
    else if algunoEnLista (dfs g (head (vecinos g x)) []) (dfs g (siguienteElemento (vecinos g x) (-1) (head (vecinos g x))) [])
    then True
    else if x > length g then False
    else False

-- funcion auxiliar que nos da el siguiente elemnto de los vecinos de un nodo, de esta forma podemos
-- comparar el dfs de el primer vecino con el dfs del segundo vecino, nos ayuda a prevenir 
-- un error, ya que en caso de que no exista un elemento siguiente, o en caso de que la lista sea
-- vacia regresa un valor por defecto, que en este caso es -1, lo cuál le permite continuar
siguienteElemento :: Eq a => [a] -> a -> a -> a
siguienteElemento [] def _ = def
siguienteElemento [_] def _ = def
siguienteElemento (x:y:xs) def buscado
    | x == buscado = y  -- Si encontramos el elemento, devolvemos el siguiente
    | otherwise = siguienteElemento (y:xs) def buscado

arbolPrueba :: Graph
arbolPrueba = [(1, [2,3]), (2, [4]), (3, [5]), (4, []), (5, [6]), (6, [])]

noEsarbolPrueba :: Graph
noEsarbolPrueba = [(1, [2,3]), (2, [4]), (3, [5,8]), (4, []), (5, [6, 7]), (6, [3]), (7, []), (8, [])]

--La siguiente funcion regresa a suma de las hojas del arbol
leafSum:: Tree Int -> Int
leafSum Empty = 0
leafSum (Node a Empty Empty) = a
leafSum (Node a b c) = leafSum b + leafSum c