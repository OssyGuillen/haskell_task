-- Universidad Simón Bolívar
-- Departamento de Computación y Tecnología de la Información
-- CI-3661 - Laboratorio de Lenguajes de Programación I
--
-- Tarea de Haskell
-- Verificador de Tautologías
-- 
-- Autores: Gabriel Iglesias 11-10476.
--		    Oscar Guillen	11-11264.


data Proposition = Bool
				 | Var
				 | Not Proposition
				 | Conj Proposition Proposition
				 | Disj Proposition Proposition
				 | Impl Proposition Proposition
				 deriving(Show,Eq)

type Environment = [(String,Bool)]
a =  [("a",True),("b",False),("c",False),("d",False),("e",False)]

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
evalP e p = Just True


vars :: Proposition -> [String]
vars p = []


isTautology :: Proposition -> Bool
isTautology p = True
