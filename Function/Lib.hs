
module Function.Lib (covid_function_t, generate_covid_ltuple) where

import Function.Matrix (transpose, inverse, dot)

import GHC.Generics


--- Function to generate matrix b which is roughly [[log (covid case cumulative)]]
getBMatrix arr = [[log y] | (x,y) <- arr]

--- Function to generate matrix A which is roughly [[1, day]]
getAMatrix arr = [[1,x] | (x,y) <- arr ]

atAInverse arr = (inverse . dot ((transpose . getAMatrix) arr)) (getAMatrix arr)

atb arr =  ((dot .transpose . getAMatrix) arr) (getBMatrix arr)

-- hasil matrix 1x2 p,a
normalFunction :: (Eq a, Floating a) => [(a, a)] -> [[a]]
normalFunction arr =  ((dot . atAInverse) arr) ((transpose . atb) arr)


--- STUB MODULE
--- First param: day type: (Int)
--- Result: return total covid cases (in t day) type: (Int)


-- --- Contoh ---
-- list_val_pbk = normalFunction [(1,2),(2,2),(3,2),(4,2),(5,4)]
-- p = list_val_pbk !! 0 !! 0
-- a = list_val_pbk !! 0 !! 1
-- b = list_val_pbk !! 0 !! 2
-- -- p =ln(k) --> k = e^p
-- k = exp (p)

-- covid_function_t :: [(Float, Float)] -> Int -> Int
-- covid_function_t xs t = round (exp p * (exp (a * (fromIntegral t :: Float))))
--                 where 
--                     p = head(normalFunction xs) !! 0
--                     a = head(normalFunction xs)!! 1
                    
--- FUNCTION ---

--- Receive list of tuples (Int, Int) type
--- First param tuple : day
--- Second param tuple : current covid cases

-- generate_covid_ltuple :: Int -> [(Float, Float)] -> [(Int, Int)]
-- generate_covid_ltuple day [] = []
-- generate_covid_ltuple day xs = a ++ [(day+1, covid_function_t xs day+1) | day <- [length xs .. day]]
--                         where a = convertFloatToIntTuple xs




convertFloatToIntTuple :: [(Int,Int)] -> [(Float, Float)]
convertFloatToIntTuple xs = [(fromIntegral x :: Float, fromIntegral y :: Float) | (x,y) <- xs]


--- STUB MODULE
--- First param: day type: (Int)
--- Result: return total covid cases (in t day) type: (Int)
covid_function_t :: [(Int,Int)] -> Int -> Int
covid_function_t days t = round (1.0015 * (exp (0.3187* (fromIntegral t :: Float))))
                    where 
                        p = head(normalFunction (convertFloatToIntTuple days)) !! 0
                        a = head(normalFunction (convertFloatToIntTuple days)) !! 1

--- FUNCTION ---

--- Receive list of tuples (Int, Int) type
--- First param tuple : day
--- Second param tuple : current covid cases
generate_covid_ltuple :: Int -> [(Int, Int)] -> [(Int, Int)]
generate_covid_ltuple day [] = []
generate_covid_ltuple day xs = xs ++ [(day+1, covid_function_t xs day+1) | day <- [length xs .. day]]
