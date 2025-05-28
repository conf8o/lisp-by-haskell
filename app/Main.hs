module Main (main) where

import Lib
import qualified S

main :: IO ()
main = do
  case sPlus of
    (S.Value (S.Integer 3)) -> putStrLn "3"
    _ -> putStrLn ""

sPlus :: S.Data
sPlus =
  S.eval $
    S.List
      ( S.Fn S.plus,
        S.List
          ( S.Value (S.Integer 1),
            S.Value (S.Integer 2)
          )
      )