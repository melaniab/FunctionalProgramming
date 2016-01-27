import Data.Char
import Data.List
--type String = [Char]

count :: Eq a => a -> [a] -> Int
--Броя срещания на първия аргумент в списъка 
count _ [] = 0
count n (x:xs) = length $ filter (\x -> x ==n) (x:xs)

matchLengths :: [[Int]] -> [[Int]] -> Bool
--Нека l1 = (a1 a2 a3 ... an) и l2 = (b1 b2 b3 ... bn). 
--Предикатът връща истина, ако разликата между дължините на всяка двойка списъци (ai, bi) е еднаква.
matchLengths [] [] = True 
matchLengths (x:xs) (y:ys) = allTheSame $ zipWith (-) (map length (x:xs)) (map length (y:ys))
	where
		allTheSame :: (Eq a) => [a] -> Bool
		allTheSame xs = and $ map (== head xs) (tail xs)



trim :: [Char] -> [Char]
--isSpace :: Char -> Bool
trim (x:xs) = filter (not . isSpace) (x:xs)


lengthEncode :: [Char] -> [[Char]]
lengthEncode str =  group str
--	where 
		--compress :: [Char] -> [Char]
		--compress x = x
		--compress(x:xs) = (x:[]) 
		


	