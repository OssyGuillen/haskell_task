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
} deriving(Show)

data Suit = Clubs | Diamonds | Spades | Hearts deriving(Show)
data Value = Numeric Int | Jack | Queen | King | Ace deriving(Show)

newtype Hand = H [Card]
	deriving(Show)

empty :: Hand
empty = H []


size :: Hand -> Int
size (H xs) = length xs