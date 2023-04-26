import Data.Char 

---------------------------------------------
-------RECURSION: FIBONACCI-------------------
---------------------------------------------

fibonacciCazuri :: Integer -> Integer 
fibonacciCazuri n 
    | n < 2     = n 
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)

fibonacciEcuational :: Integer -> Integer 
fibonacciEcuational 0 = 0 
fibonacciEcuational 1 = 1 
fibonacciEcuational n = 
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)

-- O(n) Fibonacci 
fibonacciLiniar :: Integer -> Integer 
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonacciPereche n) -- snd = second 
    where 
        fibonacciPereche :: Integer -> (Integer, Integer) -- F(n-1), F(n)
        fibonacciPereche 1 = (0, 1)
        fibonacciPereche n = (fn1, fn1 + fn2)
            where (fn2, fn1) = fibonacciPereche (n - 1)

---------------------------------------------
-------LIST RECURSIONS-------------------
---------------------------------------------

-- function that divides by 2 even numbers from a list and eliminates odd numbers
semiPareRecDestr :: [Int] -> [Int]
semiPareRecDestr l 
    | null l    = l 
    | even h    = h `div` 2 : t' 
    | otherwise = t' 
    where 
        h = head l 
        t = tail l 
        t' = semiPareRecDestr t 

semiPareRecEq :: [Int] -> [Int] 
semiPareRecEq [] = [] 
semiPareRecEq (h:t) 
    | even h    = h `div` 2 : t' 
    | otherwise = t' 
    where t' = semiPareRecEq t 

---------------------------------------------
-------LIST COMPREHENSIONS-------------------
---------------------------------------------
semiPareComp :: [Int] -> [Int]
semiPareComp l = [x `div` 2 | x <- l, even x] 

---------------------------------------------
-------EXERCISES-------------------
---------------------------------------------

-- recursion 
inIntervalRec :: Int -> Int -> [Int] -> [Int] 
inIntervalRec _ _ [] = [] 
inIntervalRec lowerBound upperBound (x:xs) 
    | lowerBound <= x && x <= upperBound    = x : (inIntervalRec lowerBound upperBound xs)
    | otherwise     = inIntervalRec lowerBound upperBound xs 
-- list comprehensions 
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp lowerBound upperBound xs = [x | x <- xs, lowerBound <= x && x <= upperBound]


-- recursion 
pozitiveRec :: [Int] -> Int 
pozitiveRec [] = 0 
pozitiveRec (h:t) 
    | h > 0     = 1 + pozitiveRec t 
    | otherwise = pozitiveRec t 
-- list comprehensions 
pozitiveComp :: [Int] -> Int 
pozitiveComp xs = length [1 | x <- xs, x > 0]


-- recursion 
pozitiiImpareRec :: [Int] -> [Int] 
pozitiiImpareRec xs = pozitiiImpareAux xs 0 
    where 
        pozitiiImpareAux [] _ = [] 
        pozitiiImpareAux (x:xs) a 
            | odd x = a : (pozitiiImpareAux xs (a + 1))
            | otherwise = pozitiiImpareAux xs (a + 1)
-- list comprehensions 
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp xs = [b | (a, b) <- zip xs [0..], odd a]


-- recursion 
multDigits :: String -> Int 
multDigits str = multDigitsRec str 1 
    where 
        multDigitsRec [] acc = acc 
        multDigitsRec (h:t) acc 
            | isDigit h = multDigitsRec t (acc * digitToInt h)
            | otherwise = multDigitsRec t acc
-- list comprehensions 
multDigitsComp :: String -> Int 
multDigitsComp sir = product [digitToInt ch | ch <- sir, isDigit ch]


-- recursion 
discountRec :: [Float] -> [Float]
discountRec [] = [] 
discountRec (x:xs)
    | discount x < 200  = discount x : discountRec xs 
    | otherwise = discountRec xs 

discount :: Float -> Float 
discount x = x - 0.25 * x 
-- list comprehensions 
discountComp :: [Float] -> [Float]
discountComp xs = [y | x <- xs, let y = discount x, y < 200]


