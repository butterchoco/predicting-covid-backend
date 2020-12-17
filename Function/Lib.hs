
module Function.Lib (covid_function_t, generate_covid_ltuple) where


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
generate_covid_ltuple day xs = xs ++ [(day, covid_function_t day) | day <- [length xs .. day]]