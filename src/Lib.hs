module Lib
    ( Node
    , createMaze
    , renderMaze
    , solveMaze
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List (find, nub)
import System.Random (RandomGen, randomR, randomRs, split)

type Node = (Int, Int)

data Adjacent = LeftNode | UpNode | BothNodes deriving (Show, Eq, Ord)

data Dir = R | L | D | U deriving (Show, Eq, Ord, Enum, Bounded)

data Maze = Maze { mazeWidth  :: Int
                 , mazeHeight :: Int
                 , mazeEdges  :: Map.Map Node Adjacent } deriving (Show, Eq)

getNeighbour :: Maze -> Node -> Dir -> Maybe Node
getNeighbour (Maze w h _) (x, y) d
  | x >  1 && x <= w && d == L = Just (x-1, y  )
  | x >= 1 && x <  w && d == R = Just (x+1, y  )
  | y >  1 && y <= h && d == U = Just (x  , y-1)
  | y >= 1 && y <  h && d == D = Just (x  , y+1)
  | otherwise                  = Nothing

getDirection :: Node -> Node -> Dir
getDirection (x1, y1) (x2, y2)
  | x1 == x2 && y1 <  y2 = D
  | x1 == x2 && y1 >  y2 = U
  | x1 <  x2 && y1 == y2 = R
  | x1 >  x2 && y1 == y2 = L
  | otherwise            = error "Can't get direction if not neighbours"

isOpenWall :: Maze -> Node -> Dir -> Bool
isOpenWall m@(Maze _ _ edges) node dir
  | isNothing neigh       = False
  | isNothing adj         = False
  | adj == Just BothNodes = True
  | dir == R || dir == L  = adj == Just LeftNode
  | dir == U || dir == D  = adj == Just UpNode
  where
    neigh = getNeighbour m node dir
    adj = Map.lookup (max node $ fromJust neigh) edges

isVisited :: Maze -> Node -> Bool
isVisited m n = any (isOpenWall m n) $ enumFrom minBound

openWall :: Maze -> Node -> Dir -> Maze
openWall m@(Maze w h edges) node dir
  | isNothing neigh                          = m
  | dir == U && hasNoAdj                     = Maze w h $ Map.insert node UpNode edges
  | dir == L && hasNoAdj                     = Maze w h $ Map.insert node LeftNode edges
  | (dir == U || dir == L) && (not hasNoAdj) = Maze w h $ Map.insert node BothNodes edges
  | dir == D                                 = openWall m (fromJust neigh) U
  | dir == R                                 = openWall m (fromJust neigh) L
  where
    neigh    = getNeighbour m node dir
    hasNoAdj = isNothing $ Map.lookup node edges

renderMaze :: Maze -> [Node] -> String
renderMaze (Maze w h edges) path = unlines $ concatMap renderRow [1..h+1]
  where
    renderRow r = [concatMap head rowString, concatMap (head . tail) rowString]
      where rowString = map (renderNode r) [1..w+1]
    renderNode y x
      | x == w+1 && y == h+1  = [ "*"   , ""     ] -- bottom right corner
      | x == w+1              = [ "*"   , "|"    ] -- right wall
      | y == h+1              = [ "*---", ""     ] -- bottom wall
      | adj == Just BothNodes = [ "*   ", "  " ++ nodeStr ++ " " ] -- left and up are open
      | adj == Just LeftNode  = [ "*---", "  " ++ nodeStr ++ " " ] -- left is open
      | adj == Just UpNode    = [ "*   ", "| " ++ nodeStr ++ " " ] -- up is open
      | otherwise             = [ "*---", "| " ++ nodeStr ++ " " ] -- left and up are closed
      where 
         adj = Map.lookup (x, y) edges
         -- FIXME: last is linear, will bomb
         nodeStr = if (x, y) == last path then "👽" else case lookup (x, y) $ zip path (tail path) of
                     Nothing -> " "
                     Just to -> dirToStr $ getDirection (x, y) to
         dirToStr d = case d of
                        L -> "←"
                        R -> "→"
                        U -> "↑"
                        D -> "↓"

randomEnumList :: (Enum a, RandomGen g) => (a, a) -> g -> [a]
randomEnumList (rangeMin, rangeMax) =
  map toEnum
  . take l
  . nub
  . randomRs (fromEnum rangeMin, fromEnum rangeMax)
  where
    l = length [rangeMin .. rangeMax]

initMaze :: Int -> Int -> Maze
initMaze w h = Maze w h Map.empty

createMaze :: (RandomGen g) => g -> Int -> Int -> Maze
createMaze gen w h = walk (gen0, initMaze w h) (startX, startY)
  where
    (gen0, gen1)   = split gen
    (startX, gen2) = randomR (1, w) gen1
    (startY, _   ) = randomR (1, h) gen2
    walk (g, maze) n = foldl (walk' n) maze neighbours
      where
        (gen3, gen4) = split g
        neighbours = mapMaybe (getNeighbour maze n) (randomEnumList (minBound, maxBound) gen3)
        walk' fromNode maze toNode
          | isVisited maze toNode = maze
          | otherwise             = walk (gen4, openWall maze fromNode dir) toNode
          where
            dir = getDirection fromNode toNode

dfs :: (Eq a, Ord a) => (a -> [a]) -> a -> a -> Maybe [a]
dfs neighboursOf start fin = fst $ dfs' Set.empty $ start
  where
    dfs' v cur
      | cur == fin = (Just [cur], v')
      | null ns    = (Nothing, v')
      | otherwise  = case foldl (\(r, v'') n -> case r of
                                      Just p  -> (Just p, v'')
                                      Nothing -> (dfs' v'' n))
                           (Nothing, v') ns of
                       (Nothing, v''') -> (Nothing, v''')
                       (Just p, v''')  -> (Just (cur : p), v''')
      where
        ns = filter (not . flip Set.member v) (neighboursOf cur)
        v' = Set.insert cur v

solveMaze :: Maze -> Node -> Node -> [Node]
solveMaze maze start fin = fromMaybe [] $ dfs (getOpenNeighbours maze) start fin
  where
    getOpenNeighbours maze node = mapMaybe (getNeighbour maze node) (filter (isOpenWall maze node) [minBound .. ])
