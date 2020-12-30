{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controllers.Home
    ( home
    , login
    , getPredictedCovidCase
    ) where

import Function.Lib (generateCovidTuple)

import           Views.Home (homeView)
import           Web.Scotty
import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics

home :: ScottyM ()
home = get "/" homeView

login :: ScottyM ()
login = get "/login" $ html "login"

--- CovidT object ---
data CovidT = CovidT {day :: Int, positive :: Int} deriving (Show, Generic)
data CovidTCases = CovidTCases { cases :: [CovidT], days_predict :: Int } deriving (Show, Generic)
instance FromJSON CovidT
instance FromJSON CovidTCases
instance ToJSON CovidTCases
instance ToJSON CovidT

--- Receive CovidT data
--- Return (Int, Int) format
case_to_int_format :: CovidT -> (Int, Int)
case_to_int_format (CovidT d p) = (d, p)

--- Receive list of CovidT data
--- Return list of (Int, Int)
all_case_format :: [CovidT] -> [(Int, Int)]
all_case_format arr = foldr (\x y -> [case_to_int_format x] ++ y) [] arr

getPredictedCovidCase :: ScottyM()
getPredictedCovidCase = post "/get-predicted-result" $ do
    response <- jsonData :: ActionM CovidTCases
    let data_all_case = cases response
    let days_to_predict = days_predict response
    let data_int_to_int = all_case_format data_all_case
    let cases = [CovidT {day = d, positive = total} | (d,total) <- generateCovidTuple days_to_predict data_int_to_int]
    json cases
