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

data Proposition = Cons Bool
				 | V String
				 | Not Proposition
				 | Conj Proposition Proposition
				 | Disj Proposition Proposition
				 | Impl Proposition Proposition
				 deriving(Show,Eq)

type Environment = [(String,Bool)]

find :: Environment -> String -> Maybe Bool
find [] k         = Nothing
find ((c,v):xs) k = if k == c then Just v
							  else find xs k


addOrReplace :: Environment -> String -> Bool -> Environment
addOrReplace e k v = reverse (foldl merge [(k,v)] e)
	where
		merge x (s,v) = if s == (fst . last) x then last x:(init x)
											   else (s,v):x

--addOrReplace e k v = reverse (inReverse (reverse e) k v)
--inReverse [] s v = [(s,v)]
--inReverse ((s,v):rs) ns nv =	if s == ns then (s,nv) : rs
--										   else (s,v) : inReverse rs ns nv

remove :: Environment -> String -> Environment
remove e k = filter (\(s,v) -> s /= k) e


evalP :: Environment -> Proposition -> Maybe Bool
evalP _ (Cons b) = Just b
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


vars :: Proposition -> [String]
vars p = nub (findVar p)
	where
		findVar (Cons b) = []
		findVar (V s) = [s]
		findVar (Not p) = findVar p
		findVar (Conj p1 p2) = findVar p1 ++ findVar p2
		findVar (Disj p1 p2) = findVar p1 ++ findVar p2
		findVar (Impl p1 p2) = findVar p1 ++ findVar p2
 
 
isTautology :: Proposition -> Bool
isTautology p = foldl (isTrue p) True $ foldr allPermutations [[]] (vars p)
	where
		isTrue p b e = fromJust (evalP e p) && b
		allPermutations str env = foldr (permute str) [] env
			where
				permute str env lenv = ((str,True):env):((str,False):env):lenv
