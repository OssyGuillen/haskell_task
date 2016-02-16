{-- Universidad Simón Bolívar.
	Departamento de Computación y Tecnología de la Información.
    CI3661 - Laboratorio de Lenguajes de Programación I
    Trimestre Enero - Marzo 2016.
   
    @author Gabriel Iglesias 11-10476.
    @author Oscar Guillen    11-11264.

-}

-- Implementación usando lista por compresión.
filterC :: (a -> Bool) -> [a] -> [a]
filterC p xs = [ x | x <- xs , p x ] 

-- Implementación usando la función map y otras funciones auxiliares.
{-filterM :: (a -> Bool) -> [a] -> [a]
filterM _ [] = []
filterM p (xs) = foldl eval [] (zip xs (map p xs))
	where	
		eval as (a, True)  = a:as
		eval as (a, False) = as
-}

-- Funcion auxiliar
comparar :: [Bool] -> [a] -> [a]
comparar [] (_:_) = []
comparar (_:_) [] = []
comparar [x] [y] =
	if x == True then [y] else [] 

comparar (x:xs) (y:ys) = 
	if x == True then y:comparar xs ys
	else comparar xs ys

filterM :: (a -> Bool) -> [a] -> [a]
filterM _ [] = []
filterM p (xs) = comparar (map p xs) xs

-- Implementación usando la función foldr.
filterF :: (a -> Bool) -> [a] -> [a]
filterF p = foldr (\x acc -> if p x then x : acc else acc) []
