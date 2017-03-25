list range = [ x | x <- [1..range-1], x `mod` 3 == 0 || x `mod` 5 == 0]

solve = sum $ list 100
