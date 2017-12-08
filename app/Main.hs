module Main where

import           Cereal
import           PrettyJSON

main :: IO ()
main = do
  print $ JObject [("good", JNumber 1), ("job", JBool False)]
