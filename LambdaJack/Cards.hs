-- Universidad Simón Bolívar
-- Departamento de Computación y Tecnología de la Información
-- CI-3661 - Laboratorio de Lenguajes de Programación I
--
-- Tarea de Haskell
-- Juego Lambdajack: Módulo Cards.
-- 
-- Autores: Gabriel Iglesias 11-10476.
--		    Oscar Guillen	11-11264.
module Cards (Hand (H), Card (Card),
			  Suit (Clubs, Diamonds, Spades, Hearts),
			  Value(Numeric, Jack, Queen, King, Ace), 
			  empty, size) where

data Card = Card {
	value :: Value,
	suit :: Suit
} 

instance Show Card where
	show (Card value suit) = show value ++ show suit 

data Suit = Clubs | Diamonds | Spades | Hearts

instance Show Suit where
	show Clubs = "♣ "
	show Diamonds = "♦ "
	show Spades = "♤ "
	show Hearts = "♥ "

data Value = Numeric Int | Jack | Queen | King | Ace
	
instance Show Value where
	show (Numeric x) = show x
	show Jack = "J "
	show Queen = "Q "
	show King = "K "
	show Ace = "A "

newtype Hand = H [Card]

instance Show Hand where
	show (H []) = ""
	show (H (a:as)) = show a ++ " " ++ show (H as) 

empty :: Hand
empty = H []


size :: Hand -> Int
size (H xs) = length xs