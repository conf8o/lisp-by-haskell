module Main (main) where

import qualified S

main :: IO ()
main = do
  case sPlus of
    S.Integer 3 -> putStrLn "3"
    _ -> putStrLn ""

sPlus :: S.Data
sPlus =
  S.eval $
    S.list
      [ S.plus,
        S.Integer 1,
        S.Integer 2
      ]