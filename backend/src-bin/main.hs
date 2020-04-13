module Main where
import Obelisk.Backend
import Web.PlayAtHome.Backend
import Web.PlayAtHome.Frontend

main :: IO ()
main = runBackend backend frontend
