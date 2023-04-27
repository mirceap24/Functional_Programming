import Data.List 
import Data.Char

-- returns list of divisors of a number 
factori :: Int -> [Int]
factori n = [d | d <- [1..n], n `mod` d == 0]

-- checks if a number is prime 
prim :: Int -> Bool 
prim n = factori n == [1, n]

-- returns list of prime numbers smaller than n 
numerePrime :: Int -> [Int]
numerePrime n = [p | p <- [2..n], prim p]

-- implement myzip which is the same as zip but has 3 arguments 
myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 [] _ _ = [] 
myzip3 _ [] _ = [] 
myzip3 _ _ [] = [] 
myzip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : myzip3 xs ys zs 

{- Prelude> map (\x -> 2 * x) [1..10]
[2,4,6,8,10,12,14,16,18,20]
Prelude> map (1 `elem`) [[2,3], [1,2]]
[False,True]
Prelude> map (`elem` [2,3]) [1,3,4,5]
[False,True,False,False]
-}

-- MAP EXERCISES
firstEl xs = map fst xs 

sumList :: [[Int]] -> [Int]
sumList = map sum 

-- prel2 :: [Int] -> Int 
-- prel2 = map prelucreaza 
--     where 
--         prelucreaza x =
--             if x `mod` 2 == 0 
--                 then x `div` 2 
--                 else x * 2 
prel2' xs = map (\x -> if even(x) then (div x 2) else x * 2) xs 

-- MAP AND FILTER EXERCISES 

stringsWithChar :: Char -> [String] -> [String]
stringsWithChar c strings = filter (elem c) strings

squaresOfOddNumbers :: [Int] -> [Int]
squaresOfOddNumbers numbers = map (^2) (filter odd numbers)

squaresOfNumbersAtOddPositions :: [Int] -> [Int]
squaresOfNumbersAtOddPositions numbers = map (square . snd) (filter (odd . fst) (zip [1..] numbers))
  where
    square x = x * x
patratImpar :: [Int] -> [Int]
patratImpar l = map (\x -> fst(x) ^ 2) $ filter (\x -> snd(x) `rem` 2 == 1) $ zip l [1..]
{- 
associate each number with its position 
filter numbers att odd positions -> odd . fst checks if the 1st elem of the tuple is odd
calculate squares of selected numbers 
-}


onlyVowels :: [String] -> [String]
onlyVowels strings = map (filter isVowel) strings 
    where 
        isVowel c = toLower c `elem` "aeiou"
numaiVocale :: [String] -> [String]
numaiVocale x = map (\a -> filter (`elem` "aeiouAEIOU") a) x


mymap :: (a -> b) -> [a] -> [b] 
mymap _ [] = [] 
mymap f (x:xs) = f x : mymap f xs 

myfilter :: (a -> Bool) -> [a] -> [a] 
myfilter _ [] = [] 
myfilter p (x:xs) 
    | p x       = x : myfilter p xs 
    | otherwise = myfilter p xs 


primeNumbersSieve :: Int -> [Int]
primeNumbersSieve n = erathostenesSieve [2..n]
    where 
        erathostenesSieve :: [Int] -> [Int]
        erathostenesSieve [] = [] 
        erathostenesSieve (x:xs) = x : erathostenesSieve [p | p <- xs, p `mod` x /= 0]


ordonataNat : [Int] -> Bool 
ordonataNat [] = True 
ordonataNat [x] = True 
ordonataNat (x:xs) = and [a < b | (a, b) <- zip (x:xs) xs]







