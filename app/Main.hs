module Main where

import Lib
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "use wasd to slide tiles"
  game <- newGame 4
  play game

play :: GameState -> IO ()
play game = do
  let board = snd game
  putStrLn $ renderGame game
  if isGameOver board
    then putStrLn "Game Over"
    else do
      dir <- getDir board
      putStrLn "\n"
      let (newScore, newBoard) = nextGameState dir game
      nextBoard <- spawnTile newBoard
      play (newScore, nextBoard)

getDir :: Board -> IO Direction
getDir board = do
  dir <- getDirInput
  if canMove dir board
    then return dir
    else getDir board

getDirInput :: IO Direction
getDirInput = do
  char <- getChar
  case char of
    'w' -> return Upward
    'd' -> return Rightward
    's' -> return Downward
    'a' -> return Leftward
    _ -> getDirInput