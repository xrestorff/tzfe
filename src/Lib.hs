module Lib where

import Control.Monad.Random (MonadRandom, fromList)
import Data.List (intercalate, intersperse, transpose)
import Data.Maybe (catMaybes, isNothing)

backslide :: [Int] -> (Int, [Int])
backslide [] = (0, [])
backslide (x : y : ys)
  | x == y =
    let (score, rest) = backslide ys
        tilePoints = x + y
     in (score + tilePoints, tilePoints : rest)
backslide (x : xs) = let (score, rest) = backslide xs in (score, x : rest)

slideLane :: [Int] -> (Int, [Int])
slideLane tiles = (score, reverse revTiles)
  where
    (score, revTiles) = backslide $ reverse tiles

slideLanes :: [[Int]] -> (Int, [[Int]])
slideLanes board = (score, lanes)
  where
    slideResults = map slideLane board
    score = sum $ map fst slideResults
    lanes = map snd slideResults

fillLane :: Int -> [a] -> [Maybe a]
fillLane len lane = nothings ++ justs
  where
    missing = len - length lane
    nothings = replicate missing Nothing
    justs = map Just lane

fillLanes :: Int -> [[a]] -> [[Maybe a]]
fillLanes len = map (fillLane len)

data Direction = Rightward | Leftward | Downward | Upward

rotateBoard :: Direction -> [[a]] -> [[a]]
rotateBoard dir = case dir of
  Rightward -> id
  Leftward -> map reverse
  Downward -> transpose
  Upward -> transpose . reverse

unrotateBoard :: Direction -> [[a]] -> [[a]]
unrotateBoard dir = case dir of
  Upward -> reverse . transpose
  _ -> rotateBoard dir

type Board = [[Maybe Int]]

type GameState = (Int, Board)

filterTiles :: Board -> [[Int]]
filterTiles = map catMaybes

nextGameState :: Direction -> GameState -> GameState
nextGameState dir (score, board) = (newScore, newBoard)
  where
    lanes = rotateBoard dir board
    tiles = filterTiles lanes
    (moveScore, slideTiles) = slideLanes tiles
    filledLanes = fillLanes (length board) slideTiles
    newScore = score + moveScore
    newBoard = unrotateBoard dir filledLanes

nextTile :: (MonadRandom m) => m Int
nextTile = fromList [(2, 0.9), (4, 0.1)]

freeSpots :: Board -> [(Int, Int)]
freeSpots board =
  [ (row, col)
    | row <- coords,
      col <- coords,
      isNothing (board !! row !! col)
  ]
  where
    coords = [0 .. length board - 1]

spawnTile :: (MonadRandom m) => Board -> m Board
spawnTile board = do
  let spots = freeSpots board
  (spawnRow, spawnCol) <- fromList (zip spots $ repeat 1)
  spawnVal <- nextTile
  let newBoard =
        zipWith
          ( \row tiles ->
              zipWith
                ( \col tile ->
                    ( if row == spawnRow && col == spawnCol
                        then Just spawnVal
                        else tile
                    )
                )
                [0 ..]
                tiles
          )
          [0 ..]
          board
  return newBoard

newGame :: (MonadRandom m) => Int -> m GameState
newGame len = do
  let emptyBoard = replicate len $ replicate len Nothing
  spawnOne <- spawnTile emptyBoard
  spawnTwo <- spawnTile spawnOne
  return (0, spawnTwo)

isFull :: Board -> Bool
isFull board = not $ any isNothing $ concat board

willScore :: Direction -> Board -> Bool
willScore dir board = score /= 0
  where
    (score, _) = slideLanes $ filterTiles $ rotateBoard dir board

canScore :: Board -> Bool
canScore board = willScore Rightward board || willScore Downward board

isGameOver :: Board -> Bool
isGameOver board = isFull board && not (canScore board)

canMove :: Direction -> Board -> Bool
canMove dir board = nextBoard /= board
  where
    (_, nextBoard) = nextGameState dir (0, board)

digitLen :: Int -> Int
digitLen int = length $ show int

renderNum :: Int -> Int -> [Char]
renderNum targetLen num = replicate padLeft ' ' ++ show num ++ replicate padRight ' '
  where
    digLen = digitLen num
    pad = targetLen - digLen
    padLeft = floor $ fromIntegral pad / 2.0
    padRight = ceiling $ fromIntegral pad / 2.0

renderRow :: Int -> [Maybe Int] -> [Char]
renderRow targetLen row = "|" ++ nums ++ "|"
  where
    renderTile = maybe (replicate targetLen ' ') (renderNum targetLen)
    nums = intercalate "|" $ map renderTile row

renderBoard :: Board -> [Char]
renderBoard board = intercalate "\n" [border, rows, border]
  where
    maxDigitLen = maximum $ map digitLen $ concat $ filterTiles board
    targetLen = maxDigitLen + 2
    border = replicate (length board * (targetLen + 1) + 1) '-'
    rows = intercalate "\n" $ intersperse border $ map (renderRow targetLen) board

renderGame :: GameState -> [Char]
renderGame (score, board) = unlines ["score: " ++ show score, "", renderBoard board]