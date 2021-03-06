-- Universidad Simón Bolívar
-- Departamento de Computación y Tecnología de la Información
-- CI-3661 - Laboratorio de Lenguajes de Programación I
--
-- Tarea de Haskell
-- Verificador de Tautologías
-- 
-- Autores: Gabriel Iglesias 11-10476.
--		    Oscar Guillen	11-11264.
import Data.Maybe 
import Data.List (foldl',nub)

data Proposition = C Bool
				 | V String
				 | Not Proposition
				 | Conj Proposition Proposition
				 | Disj Proposition Proposition
				 | Impl Proposition Proposition
				 deriving(Show,Eq)

type Environment = [(String,Bool)]


-- Busca una variable en el entorno de evaluación.
find :: Environment -> String -> Maybe Bool
find [] k         = Nothing
find ((c,v):xs) k = if k == c then Just v
							  else find xs k


-- Agrega una variable con su valor al entorno de evaluación.
-- en caso de existir, se reemplaza por el nuevo valor.
addOrReplace :: Environment -> String -> Bool -> Environment
addOrReplace e k v = reverse (foldl merge [(k,v)] e)
	where
		merge x (s,v) = if s == (fst . last) x then last x:(init x)
											   else (s,v):x


-- Remueve una variable del entorno de evaluación.
remove :: Environment -> String -> Environment
remove e k = filter (\(s,v) -> s /= k) e


-- Evalúa el valor de una proposición.
evalP :: Environment -> Proposition -> Maybe Bool
evalP _ (C b) = Just b
evalP e (V s) = find e s
evalP e (Not p) = putNot (evalP e p) 
	where
		putNot (Just a) = Just (not a)
		putNot Nothing = Nothing
evalP e (Conj p1 p2) = putConj (evalP e p1) (evalP e p2)
	where
		putConj Nothing _ = Nothing
		putConj _ Nothing = Nothing
		putConj (Just False) _ = Just False
		putConj _ (Just False) = Just False
		putConj _ _ = Just True
evalP e (Disj p1 p2) = putDisj (evalP e p1) (evalP e p2)
	where
		putDisj Nothing p = Nothing
		putDisj p Nothing = Nothing
		putDisj (Just True) _ = Just True
		putDisj _ (Just True) = Just True
		putDisj _ _ = Just False
evalP e (Impl p1 p2) = evalP e (Disj (Not p1) p2) 															


-- Consigue las variables presentes en una proposición.
vars :: Proposition -> [String]
vars p = nub (findVar p)
	where
		findVar (C b) = []
		findVar (V s) = [s]
		findVar (Not p) = findVar p
		findVar (Conj p1 p2) = findVar p1 ++ findVar p2
		findVar (Disj p1 p2) = findVar p1 ++ findVar p2
		findVar (Impl p1 p2) = findVar p1 ++ findVar p2
 

-- Verifica si una proposición es una TAUTOLOGÍA.
isTautology :: Proposition -> Bool
isTautology p = foldl' (isTrue p) True $ foldr allPermutations [[]] (vars p)
	where
		isTrue p b e = fromJust (evalP e p) && b
		-- Se van construyendo las permutaciones en el arreglo vacío.
		-- para UNA variable.
		allPermutations str env = foldr (permute str) [] env
			where
				-- Se agregan los dos posibles valores al arreglo de valores y 
				-- se agrega a la vez al arreglo de permutaciones.
				permute str env lenv = ((str,True):env):((str,False):env):lenv
