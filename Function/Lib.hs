
module Function.Lib (covid_function_t, generate_covid_ltuple) where

import Function.Matrix (transpose, inverse, dot)

import GHC.Generics


--- Function to generate matrix b which is roughly [[log (covid case cumulative)]]
getBMatrix arr = [[log y] | (x,y) <- arr]

--- Function to generate matrix A which is roughly [[1, day]]
getAMatrix arr = [[1,x^2,x] | (x,y) <- arr ]

atAInverse arr = (inverse . dot ((transpose . getAMatrix) arr)) (getAMatrix arr)

atb arr = dot ((transpose . getAMatrix) arr) (getBMatrix arr)

-- hasil matrix 1x3 p,a,b
normalFunction arr =  dot (atAInverse arr) ((transpose . atb) arr)


--- STUB MODULE
--- First param: day type: (Int)
--- Result: return total covid cases (in t day) type: (Int)

test = normalFunction [(1,2),(2,2),(3,2),(4,2),(5,4)]
p = test !! 0 !! 0
a = test !! 0 !! 1
b = test !! 0 !! 2
-- p =ln(k) --> k = e^p
k = exp (p)


covid_function_t :: Int -> Int
covid_function_t t = round (k * (exp (a * (fromIntegral t :: Float))))

--- FUNCTION ---

--- Receive list of tuples (Int, Int) type
--- First param tuple : day
--- Second param tuple : current covid cases
generate_covid_ltuple :: Int -> [(Int, Int)] -> [(Int, Int)]
generate_covid_ltuple day [] = []
generate_covid_ltuple day xs = xs ++ [(day+1, covid_function_t day+1) | day <- [length xs .. day]]
