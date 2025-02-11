[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/uUeGHGGC)

# Practica 0

> Lenguajes de Programación.  
> Semestre 2025-2.  
> Fernando Abigail Galicia Mendoza.  
> Mauricio Ayala Morales.
> Luis Mario Escobar Rosales.

## Integrantes

> Moreno Castro Fernanda - 319128727.

>  Gallegos Cortes José Antonio - 320316566. 

## Descripción

Práctica de repaso de Haskell básico.

**Fecha de entrega: 12 - febrero**

## Casos de uso:
1. buscar
 buscar [1,2,3,4,5,6,7] 2
Regresa: True
 buscar [1,2,3,4,5,6,7] 8
Regresa: False

2. sumar_lista
sumar_lista [1,2,3,4]
 Regresa: 10
sumar_lista [1,2,3,4,5]
 Regresa: 15

3. filterB
filterB (>5) [1,2,3,4,5,6,7]
 Regresa: [6,7]
filterB (<5) [1,2,3,4,5,6,7]
 Regresa: [1,2,3,4]

4. mapear (*2) [1,2,3,4,5]
 Regresa: [2,4,6,8,10]
mapear (*3) [1,2,3,4,5]
 Regresa: [3,6,9,12,15]

*Extra:* mapear_ (*2) [1,2,3,4,5]
 Regresa: [2,4,6,8,10]
mapear_ (*3) [1,2,3,4,5]
 Regresa: [3,6,9,12,15]

__Usamos arbolBinarioEjemplo, que es un arbol binario de numeros en el que 1 tiene 2 hijos, a 2 y 3, luego 2 tiene otros dos hijos, a 4 y a 5 y los demás son vacios, *es decir 3,4 y 5 son hojas.*__
__Por lo que el preorder debería ir primero a 2, luego deberia ir a 4, regresar a 2 e ir a 5, para finalmente regresar a 1 e ir a 3__

5. Preorder
preorder arbolBinarioEjemplo 
 Regresa: [1,2,4,5,3]

6. buscar 
buscar_tree arbolBinarioEjemplo 2
 Regresa: True
buscar_tree arbolBinarioEjemplo 6
 Regresa: False
 Ya que solo tenemos hasta el 5

7. alturaAB 
Esta función calcula la altura de un arbol, es decir va incrementando de 1 en 1 hasta el nodo más bajo, la raíz tiene altura 0.

alturaAB arbolBinarioEjemplo 
 Regresa: 2

*Punto Extra:* hojas
hojas arbolBinarioEjemplo 
 Regresa: 3

 8. isConnected
isConnected connectedGraph 
 Regresa: True
isConnected disconnectedGraph 
 Regresa: False

9. isTree 


10. leafSum
leafSum arbolBinarioEjemplo 
 Regresa:12
Suma 3,4 y 5