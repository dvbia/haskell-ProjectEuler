import Data.List

getIntSqrt d = ceiling (sqrt ( fromIntegral d) )

isPrime k = null [ x | x <- [2..getIntSqrt k], k `mod`x  == 0]

primeNums x = 2 : [ a | a <- [3,5..x], isPrime a ]

primeNumsTest = primeNums 1000;

--lista dzielnikow bedacych l. pierwszymi
listPrimeFactors y = filter p (primeNums y)
		where p x = y `mod` x == 0

listPrimeFactorsFaster :: (Integral a) => a -> [a] -> [a]
listPrimeFactorsFaster _ [] = []
listPrimeFactorsFaster y (x:xs)
														| y == 1 = []
														| (y `mod` x == 0) = x: listPrimeFactorsFaster (y `quot` x) (x:xs)
													  | otherwise = listPrimeFactorsFaster y xs

--temp x = nub $ listPrimeFactorsFaster x (primeNums x)
temp x = nub $ listPrimeFactorsFaster x (primeNumsTest)

--ilosc dzielnikow bedacycj l. pierwszymi
numbOfPrimeFactors x = length $ temp x

----------------------------------------------------
--test dla przykladu z zadania
checkNum y = all p (y : y+1 : y+2 : [])
	where p x = numbOfPrimeFactors x == 3

solve = find (checkNum) [1,2..]
----------------------------------------------------

--sprawdza czy 4 liczby po kolei, zaczynajac od y, spelniaja warunki zadania
checkNum2 y = all p (y : y+1 : y+2 : y+3 : [])
	where p x = numbOfPrimeFactors x == 4

solve2 = find (checkNum2) [1,2..]

main = putStrLn (show solve2)
