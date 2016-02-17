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

import Cards
import Data.Maybe

data Player = LambdaJack | You
			deriving(Show)


value :: Hand -> Int
value (H cards) = evaluate 0 cards
	where
		evaluate aces cards = snd $ foldl sumValue (aces,0) cards
			where
				sumValue (aces,i) (Card (Numeric n) _) = evalOneAce aces i n
				sumValue (aces,i) (Card Jack _) = evalOneAce aces i 10
				sumValue (aces,i) (Card King _) = evalOneAce aces i 10
				sumValue (aces,i) (Card Queen _) = evalOneAce aces i 10
				sumValue (aces,i) (Card Ace _)
					| aces == 0 = (aces+1,i + 11)
					| aces == 1 = (aces+1,i + 1 - 10)
					| otherwise = (aces+1,i + 1)


evalOneAce :: Int -> Int -> Int -> (Int,Int)
evalOneAce numAce a b = if a+b> 21 && numAce == 1 then (numAce+1,a+b-10)
											   else (numAce,a+b)


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


draw :: Hand -> Hand -> Maybe (Hand,Hand)
draw (H m) (H player) = Just (H $ tail m,H (head m:player))

--Devuelve la mano de Lambda luego de jugar su turno.
playLambda :: Hand -> Hand
playLambda h = getCard (h,empty)
	where
		getCard (m,(H [])) = getCard $ fromJust $ draw m (H [])
		getCard (m,a) 	   = if busted a || value a >= 16
							 then a
							 else getCard $ fromJust $ draw m a


--shuffle :: StdGen -> Hand -> Hand