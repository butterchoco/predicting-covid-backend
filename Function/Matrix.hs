module Function.Matrix () where

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

row :: Int -> [[a]] -> [a]
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

join :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
join f [] [] = []
join f (x:xs) (y:ys) = (f x y) : (join f xs ys)

dot :: Num a => [[a]] -> [[a]] -> [[a]]
dot x y = [[ sum (mult_row_col rx cy) | rx <- [0..row_len_min1 x]] | cy <- [0..col_len_min1 y]] where
    mult_row_col rx cy = join (*) (row rx x) (col cy y)

minor :: Eq a => Int -> Int -> [[a]] -> [[a]]
minor _ _ [] = []
minor r c x = remove_empty [[ element ir ic x | ic <- [0..col_len_min1 x], r /= ir, c /= ic] | ir <- [0..row_len_min1 x]]

remove_empty :: Eq a => [[a]] -> [[a]]
remove_empty x = filter (\a -> a /= []) x

determinant :: (Num a, Eq a) => [[a]] -> a
determinant ((x:[]):[]) = x
determinant x = sum [ ((-1) ^ ic) * (element 0 ic x) * (determinant (minor 0 ic x)) | ic <- [0..col_len_min1 x]]

cofactor :: (Num a, Eq a) => [[a]] -> [[a]]
cofactor x = [[ (-1)^(ir + ic) * (determinant(minor ir ic x)) | ic <- [0..col_len_min1 x]] | ir <- [0..row_len_min1 x]]

adjugate :: (Num a, Eq a) => [[a]] -> [[a]]
adjugate = transpose . cofactor

