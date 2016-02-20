-- Universidad Simón Bolívar
-- Departamento de Computación y Tecnología de la Información
-- CI3661 - Laboratorio de Lenguajes de Programación I
-- Trimestre Enero - Marzo 2016.
-- 
-- Tarea Haskell
-- Funciones de orden superior
--  
-- Autores: Gabriel Iglesias 11-10476.
--          Oscar Guillen    11-11264.

-- Implementación usando lista por compresión.
filterC :: (a -> Bool) -> [a] -> [a]
filterC p xs = [ x | x <- xs , p x ] 


-- Funcion auxiliar para filterM.
comparar :: [Bool] -> [a] -> [a]
comparar [] (_:_) = []
comparar (_:_) [] = []
comparar [x] [y] =
	if x == True then [y] else [] 
comparar (x:xs) (y:ys) = 
	if x == True then y:comparar xs ys
	else comparar xs ys


-- Implementación de función filter, usando función map.
filterM :: (a -> Bool) -> [a] -> [a]
filterM _ [] = []
filterM p (xs) = comparar (map p xs) xs


-- Implementación de función filter, usando la función foldr.
filterF :: (a -> Bool) -> [a] -> [a]
filterF p = foldr (\x acc -> if p x then x : acc else acc) []
