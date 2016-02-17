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

data Player = LambdaJack | You


value :: Hand -> Int
value h = 0