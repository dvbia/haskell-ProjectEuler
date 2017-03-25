import Data.List

getIntSqrt d = ceiling (sqrt ( fromIntegral d) )

isPrime k = null [ x | x <- [2..getIntSqrt k], k `mod`x  == 0]

oddComposite = [ x | x <- [3,5..], not (isPrime x) ]

christianG x = any (\n -> isPrime (x-n) ) doubleSquares		
	where doubleSquares = [ 2*n*n | n <- [1..getIntSqrt x], 2*n*n < x ] 

solve = find (\x -> not $ christianG x) oddComposite
