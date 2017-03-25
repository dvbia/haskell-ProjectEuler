getIntSqrt d = ceiling (sqrt ( fromIntegral d) )

isPrime k = null [ x | x <- [2..getIntSqrt k], k `mod`x  == 0]

primeNums x = 2 : [ a | a <- [3,5..x], isPrime a ]


