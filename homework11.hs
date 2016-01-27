listToNumber :: [Int] -> Int
listToNumber [] = error "Undefined for []"
listToNumber [x] = x
listToNumber (0 : xs) = listToNumber xs
listToNumber (x : xs) = (x * 10) ^ (length xs) + (listToNumber xs )

suffix :: (Eq a) => [a] -> [a] -> Bool
suffix [] _ = True
suffix [x] [y] = (x == y)
suffix xs1 xs2 = (last xs1 == last xs2) && suffix (init xs1) (init xs2)  


occurrences :: [Int] -> [Int] -> [Int]
occurrences [] _ = []
occurrences (x:xs) l2 = (countelem x l2) : (occurrences xs l2)
	where 
		countelem x [] = 0 
		countelem x (y:ys)
			| x == y = 1 + countelem x ys
			| otherwise = countelem x ys

removeAt :: Int -> [a] -> [a]
--removeAt _ [] = error “Greshka”
--removeAt 0 l = tail l 
removeAt n l 
	| n >= (length l) || n < 0 = error "Greshka"
	| otherwise = (take n l)  ++ (drop (n+ 1) l ) 
	
--removeAt index l = (take (index - 1) l)  ++ (drop index l ) 
