module Main where
import Frontend
import Obelisk.Backend
import Web.PlayAtHome.Backend

main :: IO ()
main = runBackend backend frontend
