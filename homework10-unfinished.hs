isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = isPrime' 2 n
        where
            isPrime' current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = isPrime' (current + 1) n
				

truncatablePrime :: Int -> Bool
truncatablePrime n 
	| n <= 9 = isPrime n
	| otherwise = isPrime n && isPrime (div n 10)
	
hasDigit:: Int -> Int -> Bool 
hasDigit n dig 
	| n < 10 = n == dig
	| otherwise = (mod n 10 == dig) || hasDigit (div n 10) dig
	
containsDigits :: Int -> Int -> Bool
--Която проверява дали всяка цифра на вторият аргумент се среща като цифра на първия аргумент.
containsDigits n1 n2
	| n2 <= 10 = hasDigit n1 n2
	| otherwise = hasDigit n1 (mod n2 10) && containsDigits n1 (div n2 10)
	
	
