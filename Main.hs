module Main where

import Board
import System.IO
--import System.Random

-- Return a character representation of a player p. It returns a Char
-- value
playerToChar:: Int -> Char
playerToChar p 
     | (p == 1) = 'O' 
     | (p == 2) = 'X'
     | otherwise = '.'

--reads lines from stdin until a positive Integer is read
getNum s = do
       putStrLn ">Enter a positive value?"
       line <- getLine
       let parsed = reads line :: [(Int, String)] in
         if length parsed == 0
         then getNum' s
         else let (x, _) = head parsed in
           if (x > 0) && (x <= s)
           then return x
           else getNum' s
       where
         getNum' s = do
           putStrLn "[]Invalid input!"
           getNum s


--Read a 1-based pair of indices (x, y) for player p, denoting an 
--unmarked place in a board bd
readXY board p = do
       putStrLn ">Enter your x-coordinate:"
       x <- getNum(size board)
       
       putStrLn ">Enter your y-coordinate:"
       y <- getNum(size board)
       
       if (isMarked(x) (y) (board))
       then invalidInput board p 
       else return (x, y)
       where
         invalidInput board p = do
           putStrLn "[]Sorry but those coordinates are already taken"
           readXY board p       

playGame gameBoard p = do
       putStrLn("[]------------------------------")
       putStrLn("[]This is player's " ++ show p ++ " turn")
       putStrLn("[]------------------------------")
       
       (x, y) <- readXY gameBoard p
       
       let gameBoard2 = mark x y gameBoard p
       
       let printableBoard = boardToStr playerToChar gameBoard2
       putStrLn printableBoard
       
       if (isGameOver gameBoard2)
       then if (isDraw gameBoard2) then return 0
            else return p
       else if (p == 1) then playGame gameBoard2 2
       else playGame gameBoard2 1
       

main = do
       let gameBoard = mkBoard(15)
       
       putStrLn("[]Welcome to Omok Game!")
       putStrLn("[]------------------------------")
       
       let printableBoard = boardToStr playerToChar gameBoard
       putStrLn printableBoard
       
       winner <- playGame gameBoard 1
       putStrLn("[]Game is finished!")
       putStrLn("[]------------------------------")
       
       if (winner == 0) then putStrLn("Its a draw!!!")
       else putStrLn("Player " ++ show winner ++ " won!!!")
       