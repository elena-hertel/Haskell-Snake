module SnakeGame where

-- Import necessary packages
import System.Random
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import System.IO
import Data.List
import Pixelated

-- Define data types
data Cell = Empty | Food | Snake deriving (Eq, Show)
type Board = [[Cell]]
type Position = (Int, Int)
type SnakeBody = [Position]
data Direction = North | South | West | East | None deriving (Eq, Show)
type Score = Int
type SnakeGame = (Board, SnakeBody, Direction, Score, StdGen)

-- Game initialization
initGame :: Int -> Int -> State StdGen SnakeGame
initGame width height = do
  gen <- get
  return (setCell (emptyBoard width height) startPos Snake, [startPos], East, 0, gen)
  where
    emptyBoard :: Int -> Int -> Board
    emptyBoard width height = replicate height (replicate width Empty)
    startPos = (width `div` 2, height `div` 2)

-- Cell modification
setCell :: Board -> Position -> Cell -> Board
setCell board (x, y) cell
  | isValidPosition board (x, y)=
      let (rowsBefore, currentRow: rowsAfter) = splitAt (y) board
          (itemsBefore, currentItem: itemsAfter) = splitAt (x) currentRow
      in rowsBefore ++ [itemsBefore ++ [cell] ++ itemsAfter] ++ rowsAfter
  | otherwise = board
  where
    isValidPosition :: Board -> Position -> Bool
    isValidPosition board (x, y) =
      x >= 0 && y >= 0 && y < length board && x < length (head board)

-- Food generation
generateFood :: SnakeGame -> SnakeGame
generateFood (board, snake, direction, score, gen) =
  let (x, newGen) = uniformR (0, length (head board) - 1) gen
      (y, newNewGen) = uniformR (0, length board - 1) newGen
      newBoard = setCell board (x, y) Food
  in if (x, y) `elem` snake then generateFood (board, snake, direction, score, newNewGen)
    else (newBoard, snake, direction, score, newNewGen)

-- Printing board & messages
printBoard :: SnakeGame -> IO ()
printBoard (board, snake, direction, score, gen) = do
  putStr "\x1b[2J\x1b[H"
  printScore score
  let border = replicate (2 * length (head board)) ' '
  putStrLn ("\x1b[48;5;0m  " ++ border ++ "  \x1b[0m")
  mapM_ printLine board
  putStrLn ("\x1b[48;5;0m  " ++ border ++ "  \x1b[0m")

printLine :: [Cell] -> IO ()
printLine cells = do
  putStr "\x1b[48;5;0m  \x1b[0m"
  mapM_ printCell cells
  putStrLn "\x1b[48;5;0m  \x1b[0m"

printScore :: Score -> IO ()
printScore score = do
  putStrLn ("Score: " ++ show score)

printCell :: Cell -> IO()
printCell cell
  | cell == Empty = putStr "\x1b[48;5;15m  \x1b[0m"
  | cell == Food = putStr "\x1b[48;5;196m  \x1b[0m"
  | cell == Snake = putStr "\x1b[48;5;47m  \x1b[0m"

printGameOver :: SnakeGame -> IO ()
printGameOver (board, snake, direction, score, gen) = do
  putStr "\x1b[2J\x1b[H" -- clear screen
  printWord (toChars "game over")
  putStrLn "\n"
  printWord (toChars $ "score: " ++ (show score))

printSnake :: IO ()
printSnake = do
  putStr "\x1b[2J\x1b[H"
  printWord [s,n,a,k,e]

printWin :: IO ()
printWin = do
  putStr "\x1b[2J\x1b[H"
  printWord (toChars "you won!")

-- Game Logic
collision :: Board -> [Position] -> Bool
collision board snake =
  let (x, y) = head snake
  in x < 0 || x >= length board || y < 0 || y >= length (head board) || (x, y) `elem` (tail snake)

updateDirection :: Char -> SnakeGame -> SnakeGame
updateDirection 'i' (board, snake, _, score, gen) = (board, snake, North, score, gen)
updateDirection 'k' (board, snake, _, score, gen) = (board, snake, South, score, gen)
updateDirection 'j' (board, snake, _, score, gen) = (board, snake, West, score, gen)
updateDirection 'l' (board, snake, _, score, gen) = (board, snake, East, score, gen)
updateDirection _ game = game

moveSnake :: SnakeGame -> SnakeGame
moveSnake (board, snake, direction, score, gen) =
  if collision board ((fst newHead, snd newHead) : snake) then (board, snake, None, score, gen)
  else
    if (board !! (snd newHead) !! (fst newHead)) == Food then -- snake grows by 1, score increases by 1
      let newSnake = newHead : snake
          newBoard = setCell board newHead Snake
      in generateFood (newBoard, newSnake, direction, score + 1, gen)
    else -- snake continues to move in direction
      let newSnake = newHead : (init snake)
          newBoard = setCell (setCell board newHead Snake) (last snake) Empty
      in (newBoard, newSnake, direction, score, gen)
  where (x, y) = head snake
        newHead = case direction of
          North -> (x, y - 1)
          South -> (x, y + 1)
          East ->  (x + 1, y)
          West -> (x - 1, y)

-- Game Loop
gameLoop :: SnakeGame -> IO ()
gameLoop game = do
  let (board, snake, direction, score, gen) = game
  if direction == None then do -- if collision occured, GAME OVER
    printGameOver (board, snake, direction, score, gen)
  else if (length snake == (length (head board)) * length(board)) then
    printWin
  else do
    inputAvailable <- hReady stdin -- allows game loop to continue while waiting for input
    if inputAvailable
      then do
        key <- getChar
        let newGame = moveSnake (updateDirection key game)
        printBoard game
        gameLoop newGame
    else do
      let newGame = moveSnake game
      printBoard newGame
      threadDelay 150000
      gameLoop newGame

-- Main
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  printSnake
  putStrLn "Use IJKL keys to control snake"
  putStrLn "Press SPACE BAR to begin"
  space <- getChar
  if space == ' ' then do
    stdGen <- initStdGen
    let width = 15
        height = 15
        initialGame = evalState (initGame width height) stdGen
    gameLoop (generateFood initialGame)
  else putStrLn "Invalid input. Press SPACE BAR to begin"