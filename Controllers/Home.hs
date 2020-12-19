{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controllers.Home
    ( home
    , login
    , getPredictedCovidCase
    ) where

import Function.Lib (generate_covid_ltuple)

import           Views.Home (homeView)
import           Web.Scotty (ScottyM, get, html, json)
import           Data.Aeson (ToJSON)
import           GHC.Generics

home :: ScottyM ()
home = get "/" homeView

login :: ScottyM ()
login = get "/login" $ html "login"

--- CovidT object ---
data CovidT = CovidT {day :: Int, total_case :: Int} deriving (Show, Generic)
instance ToJSON CovidT

getPredictedCovidCase :: ScottyM()
getPredictedCovidCase = get "/get-predicted-result"
                         $ json [CovidT {day = d, total_case = total} | (d,total) <- generate_covid_ltuple 10 [(1,2),(2,3)]]
