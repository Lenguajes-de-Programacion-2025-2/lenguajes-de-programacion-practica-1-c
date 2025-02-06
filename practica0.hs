module Practica0 where 


{-- Recursion y recursion de Cola --}

--Funcion buscar : Dada una lista de Enteros y elemento , Regresa verdadero en caso de que el elemento se encuentre en la lista
--En otro caso regresa False 

buscar::[Int]->Int->Bool
buscar [] _ = False
buscar (x:xs) e
  | x == e    = True
  | otherwise = buscar xs e


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
filterB p (x:xs)
  | p x       = x : filterB p xs
  | otherwise = filterB p xs

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
buscar_tree:: Tree a -> a -> Bool
buscar_tree Empty e  =  error "Sin implementar"


--Punto Extra:  Implementa  una funcion que cuente la cantidad de hojas del arbol 
hojas:: Tree a -> Int
hojas Empty  = error "Sin implementar"


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
isConnected [] = error "Sin implementar"

--Ejemplos

connectedGraph :: Graph
connectedGraph = [(1, [2,3]), (2, [4]), (3, [4,5]), (4, [6]), (5, [6]), (6, [])]  --Debe Regresar True

disconnectedGraph :: Graph
disconnectedGraph = [(1, [2]), (2, [1]), (3, [4]), (4, [3])] --Debe regresar False 


--La siguiente funcion verfiica que la grafica es un arbol 
--Tip : Recuerda que un arbol es una grafica conexa y sin ciclos
isTree :: Graph -> Bool
isTree []  = error "Implementar"


--La siguiente funcion regresa a suma de las hojas del arbol
leafSum:: Tree Int -> Int 
leafSum Empty = error "Sin implementar "