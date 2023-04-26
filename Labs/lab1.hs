-- INTRO TO HASKELL

import Data.List 

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer 
double x = x + x 

triple :: Integer -> Integer 
triple x = x + x + x 

penta :: Integer -> Integer 
penta x = x + x + x + x + x 

maxim x y = if (x > y)
                then x 
            else 
                y 

max3 x y z = let 
            u = maxim x y 
            in (maxim u z)

-- sum of squares of two numbers 
squareSum :: Integer -> Integer -> Integer 
squareSum x y = x^2 + y^2 

-- checks if a number is odd or even 
isEven :: Integer -> String 
isEven n = if n `mod` 2 == 0 then "even" else "odd"

-- factorial of a number 
factorial :: Integer -> Integer 
factorial n = product [1..n]

-- checks if the 1st parameter is greater than the double of the 2nd parameter
verifyDouble :: Integer -> Integer -> Bool 
verifyDouble a b = a > double 
    where double = 2 * b

-- Rock-Paper-Scissors game 
data Alegere = Piatra | Foarfeca | Hartie deriving (Eq, Show)
data Rezultat = Victorie | Infrangere | Egalitate deriving (Eq, Show)

partida :: Alegere -> Alegere -> Rezultat 
partida Piatra Foarfeca = Victorie 
partida Foarfeca Hartie = Victorie 
partida Hartie Piatra = Victorie 
partida alegere1 alegere2 
    | alegere1 == alegere2 = Egalitate 
    | otherwise = Infrangere

