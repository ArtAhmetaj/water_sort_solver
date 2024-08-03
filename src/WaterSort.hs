module WaterSort (Container (..), Stack (..),Color (..), ColorAmount (..),solveGame) where

import Data.List (nub)
import qualified Data.Set as Set
import Stack

maximumColors :: Int
maximumColors = 4

type Index = Int

data Color = Yellow | Blue | Purple | Orange | Green | Red | DarkGreen | Gray | Coffee | Pink | LightGreen | DarkBlue | Cream deriving (Show, Eq, Ord)

type Move = (Index, Index)

data ColorAmount = ColorAmount Color Int deriving (Show, Eq, Ord)

data Container = Container Index (Stack ColorAmount) deriving (Eq, Show, Ord)

type Visited = Set.Set Containers

type Containers = [Container]

type Answer = [Move]

removeColor :: Container -> (Maybe ColorAmount, Container)
removeColor (Container index (Stack (x : xs))) = (Just x, Container index (Stack xs))
removeColor (Container index s@(Stack [])) = (Nothing, Container index s)

mergeStackColors :: ColorAmount -> Stack ColorAmount -> Stack ColorAmount
mergeStackColors newColor@(ColorAmount c value) (Stack (a@(ColorAmount c2 value2) : xs))
  | c == c2 = Stack (mergedColor : xs)
  | otherwise = Stack (a : stackValues (mergeStackColors newColor (Stack xs)))
  where
    mergedColor = ColorAmount c (value + value2)
mergeStackColors newColor (Stack []) = Stack [newColor]

addColor :: Maybe ColorAmount -> Container -> Container
addColor colorAmount a@(Container cIndex stack) = case colorAmount of
  Just x -> Container cIndex (mergeStackColors x stack)
  Nothing -> a

calculateColors :: [ColorAmount] -> Int
calculateColors colors = sum $ map (\(ColorAmount _ v) -> v) colors

containerhasSpace :: Container -> Container -> Bool
containerhasSpace (Container _ (Stack (ColorAmount _ e : _))) (Container _ (Stack colors)) = calculateColors colors + e <= maximumColors
containerhasSpace (Container _ (Stack [])) _ = False

colorFilled :: Maybe ColorAmount -> Bool
colorFilled (Just (ColorAmount _ value)) = value == maximumColors
colorFilled Nothing = False

isFilledContainer :: Container -> Bool
isFilledContainer (Container _ (Stack colors@(x : _))) = length (nub colors) == 1 && colorFilled (Just x)
isFilledContainer _ = False

isEmptyContainer :: Container -> Bool
isEmptyContainer (Container _ (Stack [])) = True
isEmptyContainer _ = False

isGameOver :: Containers -> Bool
isGameOver = all (\x -> isFilledContainer x || isEmptyContainer x)

getValidMoves :: Containers -> [Move]
getValidMoves containers =
  [getIndexes f s | f <- containers, s <- containers, isValidMove f s]
  where
    getIndexes (Container v1 _) (Container v2 _) = (v1, v2)

sameTopColors :: Container -> Container -> Bool
sameTopColors (Container _ (Stack ((ColorAmount color1 _) : _))) (Container _ (Stack ((ColorAmount color2 _) : _))) = color1 == color2
sameTopColors (Container _ (Stack [])) _ = False
sameTopColors (Container _ (Stack _)) (Container _ (Stack [])) = True

uselessEmptyMove :: Container -> Container -> Bool
uselessEmptyMove (Container _ (Stack colors)) (Container _ (Stack [])) = length (nub colors) == 1
uselessEmptyMove _ _ = False

isValidMove :: Container -> Container -> Bool
isValidMove firstContainer secondContainer =
  (firstContainer /= secondContainer)
    && sameTopColors firstContainer secondContainer
    && containerhasSpace firstContainer secondContainer
    && notDoneContainers
    && not (uselessEmptyMove firstContainer secondContainer)
  where
    notDoneContainers = not $ any isFilledContainer [firstContainer, secondContainer]

-- TODO: check on removing index from datatype
performMove :: Containers -> Move -> Containers
performMove containers (from, to) = map updateContainer containers
  where
    (maybeColor, newFromContainer) = removeColor (containers !! from)
    
    updateContainer container@(Container index _)
      | index == to = addColor maybeColor container
      | index == from = newFromContainer
      | otherwise = container


-- TODO: maybe remove Maybe Answer to Answer and if no result return [] for this
solveGame :: Containers -> Maybe Answer
solveGame containers = dfs containers (Just []) Set.empty

dfs :: Containers -> Maybe Answer -> Visited -> Maybe Answer
dfs containers solution visited
  | null solution = Nothing
  | isGameOver containers = solution -- Found a solution
  | containers `Set.member` visited = Nothing -- Prune if the state has been visited
  | otherwise = exploreMoves possibleMoves updatedVisited
  where
    possibleMoves = getValidMoves containers
    updatedVisited = Set.insert containers visited

    exploreMoves :: [Move] -> Visited -> Maybe Answer
    exploreMoves [] _ = Nothing -- No more moves to explore
    exploreMoves (move : moves) visited' =
      let newContainers = performMove containers move
          newSolution = fmap (\s -> s ++ [move]) solution
       in case dfs newContainers newSolution visited' of
            Just answer -> Just answer
            Nothing -> exploreMoves moves visited'