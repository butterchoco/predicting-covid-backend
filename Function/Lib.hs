
module Function.Lib (covid_function_t, generate_covid_ltuple) where


import Function.Matrix (transpose, inverse, dot)

import GHC.Generics

--- STUB MODULE
--- First param: day type: (Int)
--- Result: return total covid cases (in t day) type: (Int)
covid_function_t :: Int -> Int
covid_function_t t = round (1.0015 * (exp (0.3187* (fromIntegral t :: Float))))


--- FUNCTION ---

--- Receive list of tuples (Int, Int) type
--- First param tuple : day
--- Second param tuple : current covid cases
generate_covid_ltuple :: Int -> [(Int, Int)] -> [(Int, Int)]
generate_covid_ltuple day [] = []
generate_covid_ltuple day xs = xs ++ [(day+1, covid_function_t day+1) | day <- [length xs .. day]]

--- Function to generate matrix b which is roughly [[log (covid case cumulative)]]
getBMatrix arr = [[log y] | (x,y) <- arr]

--- Function to generate matrix A which is roughly [[1, day]]
getAMatrix arr = [[1,x^2,x] | (x,y) <- arr ]

atAInverse arr = (inverse . dot ((transpose . getAMatrix) arr)) (getAMatrix arr)

atb arr = dot ((transpose . getAMatrix) arr) (getBMatrix arr)

-- hasil matrix 1x3 p,a,b
normalFunction arr =  dot (atAInverse arr) ((transpose . atb) arr)
