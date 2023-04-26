-- maximum function implemented recursively 
maximum' :: (Ord a) => [a] -> a 
maximum' [] = error "maximum of empty list"
maximum' [x] = x 
maximum' (x:xs)
    | x > maxTail = x 
    | otherwise = maxTail 
    where maxTail = maximum' xs 
{- maximum' [2, 5, 1]
list will be split in 2 and [5, 1]
where clause wants to know the maximum of [5,1]
[5,1] is split into 5 and [1]
where clause wants to know the maximum of [1]
edge condition => returns 1 
-}
max' :: (Ord a) => [a] -> a 
max' [] = error "maximum of empty list"
max' [x] = x 
max' (x:xs) = max x (max' xs)
{-
max' [2, 5, 1] 
= max 2 (max' [5, 1])
= max 2 (max' 5 (max' [1]))
* max' [1] = 1
-}

-- replicate function implemented recursively 
replicate' :: (Num i, Ord i) => i -> a -> [a] 
replicate' n x 
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x 

-- take function implemented recursively 
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n - 1) xs 

-- reverse function implemented recursively 
reverse' :: [a] -> [a] 
reverse' [] = [] 
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat function implemented recursively
repeat' :: a -> [a] 
repeat' x = x : repeat' x  

-- zip function implemented recursively 
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = [] 
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

-- elem function implemented recursively 
elem' :: (Eq a) => a -> [a] -> Bool 
elem' a [] = False 
elem' a (x:xs)
    | a == x    = True 
    | otherwise = a `elem'` xs 

-- QUICKSORT 
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] 
quicksort (x:xs) =  
    let smallerSorted   = quicksort [a | a <- xs, a <= x]
        biggerSorted    = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
{-
[5,1,9,4,6,7,3]
* [1,4,3] ++ [5] ++ [9,6,7]
* [] ++ [1] ++ [4, 3] ++ [5] ++ [6, 7] ++ [9] ++ []
* [] ++ [1] ++ [3] ++ [4] ++ [] ++ [5] ++ [] ++ [6] ++ [7] ++ [9] ++ []
* [] ++ [1] ++ [] ++ [3] ++ [] ++ [4] ++ [] ++ [5] ++ [] ++ [6] ++ [] ++ [7] ++ [] ++ [9] ++ []
-}

