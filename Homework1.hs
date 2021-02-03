{-allEven to check if a number is odd or even-}
allEven :: [Int] -> Bool
allEven [] = True
allEven (x:xs) = if x `mod` 2 == 0 then allEven xs else False 

{-multiplying with index-}
fun :: [Int] -> Int -> [Int]
fun [] i = []
fun (x:xs) i = [x*i] ++ fun xs (i+1)


f :: [Int] -> [Int]
f i = fun i 0 


{- function divisibleBy produces the (infinite) list of all integers greater than or equal to one that are exactly divisible by n. -}
divisibleby :: Int -> [Int]
divisibleby n = [x | x <- [1..], x `mod` n == 0 , x>=1] 

mysum = sum(take 10 (divisibleby 7))  {- Function mysum gives the sum of the first 10 numbers
                                         greater than or equal to one that are divisible by 7. 
                                         Take takes first 10 elements from the infinite list from 
                                         function divisibleby and sum calculates the sum of those elements -}