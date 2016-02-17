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
	value :: Value,
	suit :: Suit
}

data Suit = Clubs | Diamonds | Spades | Hearts
data Value = Numeric Int | Jack | Queen | King | Ace

newtype Hand = H [Card]

empty :: Hand
empty = H []


size :: Hand -> Int
size (H xs) = length xs