-- Práctica 2 - Listas con recursión y listas por comprensión

-- 1. Listas con recursión

-- 1.1. Obtener la longitud de una lista.
-- Se define la longitud de la lista como 0 para la lista vacía
-- y para una lista no vacía se suma 1 al resultado de la longitud del resto.
longitud :: [a] -> Int 
longitud []     = 0
longitud (_:xs) = 1 + longitud xs

-- 1.2. Sumar todos los números de una lista.
-- La suma de la lista vacía es 0; en otro caso, se suma el primer elemento
-- al resultado recursivo de la suma de la cola.
sumaLista :: Num a => [a] -> a 
sumaLista []     = 0
sumaLista (x:xs) = x + sumaLista xs

-- 1.3. Agregar un elemento a una lista.
-- Se recibe una lista, un elemento y un valor booleano.
-- Si el booleano es True, se inserta el elemento al principio usando el operador (:)
-- y si es False, se inserta al final utilizando (++).
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento xs x True  = x : xs
agregaElemento xs x False = xs ++ [x]

-- 1.4. Obtener el máximo de una lista.
-- Se asume que la lista no está vacía.
-- El máximo de una lista con un solo elemento es ese elemento,
-- y en el caso de una lista mayor, se compara el primer elemento con el máximo del resto.
maximoLista :: (Ord a, Num a) => [a] -> a
maximoLista [x]    = x
maximoLista (x:xs) = max x (maximoLista xs)

-- 1.5. Recuperar un elemento de una lista de acuerdo a su índice.
-- Se retorna el elemento en la posición indicada; si el índice es negativo o
-- mayor o igual a la longitud de la lista, se lanza un error con el mensaje "Índice no válido".
recuperarElemento :: [a] -> Int -> a
recuperarElemento [] _ = error "Índice no válido"
recuperarElemento (x:_) 0 = x
recuperarElemento (x:xs) n
  | n < 0     = error "Índice no válido"
  | otherwise = recuperarElemento xs (n - 1)

-- 2. Listas por comprehensión

-- 2.1. Recuperar un elemento de una lista de acuerdo a su índice..
-- Se genera la lista [1..n] y se filtran aquellos números que dividen exactamente a n.
divisoresDeN :: Int -> [Int]
divisoresDeN n = [ x | x <- [1..n], n `mod` x == 0 ]

-- 2.2. Convertir una lista a conjunto..
-- Se toma el primer elemento y se filtran todas sus ocurrencias en el resto de la lista,
-- repitiendo el proceso recursivamente.
conjuntoLista :: Eq a => [a] -> [a]
conjuntoLista []     = []
conjuntoLista (x:xs) = x : conjuntoLista (filter (/= x) xs)

-- 2.3. Obtener los números pares de una lista.
-- Se utiliza una lista por comprehensión para incluir solo aquellos elementos
-- que cumplen que son pares.
soloPares :: [Int] -> [Int]
soloPares xs = [ x | x <- xs, even x ]

