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

row_len_min1 :: [a] -> Int
row_len_min1 [] = -1
row_len_min1 x = length x - 1

col_len_min1 :: [[a]] -> Int
col_len_min1 [] = -1
col_len_min1 x = (length . transpose) x - 1

calculate :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
calculate f [] [] = []
calculate f [] _ = error "Both arrays' length should be equal"
calculate f _ [] = error "Both arrays' length should be equal"
calculate f (x:xs) (y:ys) = (f x y) : (calculate f xs ys)

dot :: Num a => [[a]] -> [[a]] -> [[a]]
dot x y = [[ sum (mult_row_col rx cy) | rx <- [0..row_len_min1 x]] | cy <- [0..col_len_min1 y] ] where
    mult_row_col rx cy = calculate (*) (row rx x) (col cy y)

remove_empty :: Eq a => [[a]] -> [[a]]
remove_empty x = filter (\a -> a /= []) x

minor :: Eq a => Int -> Int -> [[a]] -> [[a]]
minor _ _ [] = []
minor r c x = remove_empty [[ element ir ic x | ir <- [0..row_len_min1 x], r /= ir, c /= ic] | ic <- [0..col_len_min1 x] ]
