
module Function.Lib (covidFunctionT, generateCovidTuple) where

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




convertFloatToIntTuple :: [(Int,Int)] -> [(Float, Float)]
-- convertFloatToIntTuple xs = [(fromIntegral x :: Float, fromIntegral y :: Float) | (x,y) <- xs]
convertFloatToIntTuple = map (\x -> (fromIntegral (fst x) ::Float, fromIntegral (snd x) :: Float))


-- Example --
-- list_val_pbk = normalFunction [(1,2),(2,2),(3,2),(4,2),(5,4)]
-- p = list_val_pbk !! 0 !! 0
-- a = list_val_pbk !! 0 !! 1
-- b = list_val_pbk !! 0 !! 2
-- -- p =ln(k) --> k = e^p
-- k = exp (p)

--- STUB MODULE
--- First param: day type: (Int)
--- Result: return total covid cases (in t day) type: (Int)
covidFunctionT :: [(Int,Int)] -> Int -> Int
covidFunctionT days t = round (exp p * exp (a* (fromIntegral t :: Float)))
                    where 
                        p = head(normalFunction (convertFloatToIntTuple days)) !! 0
                        a = head(normalFunction (convertFloatToIntTuple days)) !! 1

--- FUNCTION ---

--- Receive list of tuples (Int, Int) type
--- First param tuple : day
--- Second param tuple : current covid cases
generateCovidTuple :: Int -> [(Int, Int)] -> [(Int, Int)]
generateCovidTuple day [] = []
generateCovidTuple day xs = xs ++ [(day+1, covidFunctionT xs day+1) | day <- [length xs .. day]]
