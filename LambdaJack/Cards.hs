-- Universidad Simón Bolívar
-- Departamento de Computación y Tecnología de la Información
-- CI-3661 - Laboratorio de Lenguajes de Programación I
--
-- Tarea de Haskell
-- Juego Lambdajack: Módulo Cards.
-- 
-- Autores: Gabriel Iglesias 11-10476.
--		    Oscar Guillen	11-11264.
module Cards where

data Card = Card {
	lvalue :: Value,
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
	show (Numeric x) = show x ++ " "
	show Jack = "J "
	show Queen = "Q "
	show King = "K "
	show Ace = "A "

newtype Hand = H [Card]
	deriving(Show)

empty :: Hand
empty = H []


size :: Hand -> Int
size (H xs) = length xs