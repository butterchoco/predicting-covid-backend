module Function.Matrix () where

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

row :: Int -> [[a]] -> [a]
row _ [] = error "Matrix size (row) should be > n"
row n (x:xs) | n <= 0 = x
             | otherwise = row (n-1) xs

col :: Int -> [[a]] -> [a]
col n = map (head . drop n)

element :: Int -> Int -> [[a]] -> a
element r c x = (head . col c) [(row r x)]