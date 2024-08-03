module Main (main) where

import qualified Data.Set as Set

import WaterSort
    ( solveGame,
      Stack(Stack),
      Color(..),
      ColorAmount(ColorAmount),
      Container(..) )



containers :: [Container]
containers =
  [Container 0 (Stack [ColorAmount Blue 1,ColorAmount Red 2,ColorAmount LightGreen 1]),
  Container 1 (Stack [ColorAmount Green 1, ColorAmount Purple 1, ColorAmount Pink 1, ColorAmount Red 1]),
  Container 2 (Stack [ColorAmount Purple 1,ColorAmount Red 1, ColorAmount DarkGreen 1, ColorAmount Cream 1]),
  Container 3 (Stack [ColorAmount Orange 1,ColorAmount DarkBlue 1, ColorAmount Blue 1, ColorAmount Yellow 1]),
  Container 4 (Stack [ColorAmount Pink 1, ColorAmount DarkGreen 1,ColorAmount Cream 1, ColorAmount Yellow 1]),
  Container 5 (Stack [ColorAmount Green 1, ColorAmount Gray 2, ColorAmount Yellow 1]),

  Container 6 (Stack [ColorAmount Blue 1, ColorAmount DarkGreen 1,ColorAmount Gray 1,ColorAmount Yellow 1]),
  Container 7 (Stack [ColorAmount LightGreen 1, ColorAmount Green 1,ColorAmount LightGreen 1,ColorAmount Orange 1]),
  Container 8 (Stack [ColorAmount LightGreen 1, ColorAmount DarkBlue 1,ColorAmount Cream 1,ColorAmount Blue 1]),
  Container 9 (Stack [ColorAmount Gray 1,ColorAmount DarkBlue 1,ColorAmount Purple 1,ColorAmount Pink 1]),
  Container 10 (Stack [ColorAmount DarkBlue 1,ColorAmount Pink 1, ColorAmount Orange 1, ColorAmount Purple 1]),
  Container 11 (Stack [ColorAmount DarkGreen 1, ColorAmount Cream 1, ColorAmount Orange 1, ColorAmount Green 1 ]),
  Container 12 (Stack []),
  Container 13 (Stack [])
  ]

noSolutionResponse :: String
noSolutionResponse = "No Solution"


main :: IO ()
main = do
    putStrLn ("Started calculting for solution for input:" ++ show containers)
    let solution = solveGame containers
    case solution of
        Just x -> if null x then putStrLn noSolutionResponse else print x
        Nothing -> putStrLn noSolutionResponse

