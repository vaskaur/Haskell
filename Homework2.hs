{-Switching neighbouring elements-}
switch :: [a] -> [a]
switch [] = []
switch [x] = [x]
switch (x:xs) = head xs : x : switch (tail xs) -- switching the list by taking head and tail of the list

{-reversing list-}
listRev :: [a] -> [a]
listRev [] = []
listRev [x] =[x]
listRev xs = foldr (\x acc -> acc ++ [x]) [] xs -- list reversal. Using ++ to append element to the end of a list


filterXOR :: (a-> Bool) -> (a-> Bool) -> [a] -> [a]
filterXOR p q [] = []
filterXOR p q (x:xs) = if ( (p x) || (q x) ) && not ( (p x) && (q x) ) then (x : filterXOR p q xs) else filterXOR p q xs 
{-filter the list using XOR operation. The XOR returns rue if one from the two cases is true and false if both the cases are true.-} 
