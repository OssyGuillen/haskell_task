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
} deriving(Show)


size :: Hand -> Int
size (H xs) = length xs

data Suit = Clubs | Diamonds | Spades | Hearts deriving(Show)
data Value = Numeric Int | Jack | Queen | King | Ace deriving(Show)

newtype Hand = H [Card]
	deriving(Show)

data Player = LambdaJack | You
			deriving(Show)


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


busted :: Hand -> Bool
busted h = value h > 21


winner :: Hand -> Hand -> Player
winner player lambda
	| not (busted player) && not (busted lambda) = who (value player) (value lambda)
	| busted player && not(busted lambda) = LambdaJack
	| not(busted player) && busted lambda = You
	| otherwise = LambdaJack
	where
		who p l
			| p > l = You
			| p < l = LambdaJack
			| otherwise = LambdaJack


fullDeck :: Hand
fullDeck = H $ merge [Card (Numeric n) y | n <- [2..10], 
										   y <- [Clubs,Diamonds,Spades,Hearts]] 
					 [Card x y | x <- [Jack,Queen,King,Ace], 
						         y <- [Clubs,Diamonds,Spades,Hearts]]
	where
		merge a b = a++b