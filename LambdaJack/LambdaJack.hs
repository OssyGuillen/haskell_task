-- Universidad Simón Bolívar
-- Departamento de Computación y Tecnología de la Información
-- CI-3661 - Laboratorio de Lenguajes de Programación I
--
-- Tarea de Haskell
-- Juego Lambdajack: Módulo LambdaJack.
-- 
-- Autores: Gabriel Iglesias 11-10476.
--		    Oscar Guillen	11-11264.
module LambdaJack where

data Card = Card {
	lvalue :: Value,
	suit :: Suit
}
data Suit = Clubs | Diamonds | Spades | Hearts
data Value = Numeric Int | Jack | Queen | King | Ace

newtype Hand = H [Card]

data Player = LambdaJack | You


value :: Hand -> Int
value (H cards) = if (evaluate 11 cards) > 21 then evaluate 1 cards
											  else evaluate 11 cards
	where
		evaluate ace cards = foldl (sumValue ace) 0 cards
			where
				sumValue ace i (Card (Numeric n) _) = i + n
				sumValue ace i (Card Jack _) = i + 10
				sumValue ace i (Card King _) = i + 10
				sumValue ace i (Card Queen _) = i + 10
				sumValue ace i (Card Ace _) = i + ace