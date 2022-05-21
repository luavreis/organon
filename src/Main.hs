module Main where
import Render
import Site.Roam

main :: IO ()
main = runHeistSite @RoamRoute ()
