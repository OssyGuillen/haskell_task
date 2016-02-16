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
size h =  0