module Board where 

-- Return an empty nxn board, where n is a positive number. A 1-based 
-- pair of indices (x,y) will be used to access a specific place of
-- the board, where x and y are column and row indices, respectively
mkBoard:: Int -> [Int]
mkBoard n = take (n * n) (repeat 0)

--Creates and returns the first player
mkPlayer:: Int
mkPlayer = 1 

-- Creates and returns the second player
mkOponent:: Int 
mkOponent = 2

-- Return the size of a board bd, n for an nxn board
size:: [Int] -> Int
size board = floor . sqrt . fromIntegral . length $ board

-- Return a row x of a board bd, where x is a 1-based index. It returns
-- a list of size n, where n is the size of bd
row:: Int -> [Int] -> [Int]
row x board
     | x <= 0 = []
     | x == 1 = take (size board) board
     | otherwise = take (size board) (drop ((x - 1) * size board) (board))

-- Returns a column y of a board bd, where y is a 1-based index. It
-- returns a list of size n, where n is the size of bd
column:: Int -> [Int] -> [Int]
column y board
     | y <= 0 = []
     | y <= (length board) = board !! (y - 1) : column (y + size board) board
     | otherwise = []

-- Helper function that returns the right diagonal of a board bd at index x y
-- where x and y are a 1-based indexes. It returns a list with all the diagonal elements
rightDiagonal:: Int -> Int -> [Int] -> [Int]
rightDiagonal x y board
     | (x > size board) || (y > size board) = []
     | (x == size board) || (y == size board) = [(row x board) !! (y - 1)]
     | otherwise = (row x board) !! (y - 1) : rightDiagonal (x + 1) (y + 1) board 

-- Helper function that returns the left diagonal of a board bd at index x y
-- where x and y are a 1-based indexes. It returns a list with all the diagonal elements
leftDiagonal:: Int -> Int -> [Int] -> [Int]
leftDiagonal x y board
     | (x > size board) || (y > size board) = []
     | (x == size board) || (y == 1) = [(row x board) !! (y - 1)]
     | otherwise = (row x board) !! (y - 1) : leftDiagonal (x + 1) (y - 1) board 


-- Marks a place (x,y) in a board bd by a player p, where x and y 
-- are 1-based column and row indices. The specified place is assumed
-- to be empty
mark:: Int -> Int -> [Int] -> Int -> [Int]
mark _ _ [] _ = []
mark x y board p = let (h, _:t) = splitAt (((x - 1) * size board) + (y - 1)) board
                in h ++ p : t

-- Determines if a place (x,y) of a board bd unmarked or a stone not placed 
-- The x and y are 1-based column and row indices
isEmpty:: Int -> Int -> [Int] -> Bool
isEmpty x y board
     | (row x board) !! (y - 1) == 0 = True
     | otherwise = False

-- Determines if a place (x,y) of a board bd have a stone placed The x and y 
-- are 1-based column and row indices 
isMarked:: Int -> Int -> [Int] -> Bool
isMarked x y board = not (isEmpty x y board)

-- Determines if a place (x,y) of a board bd have a stone placed by a player p
-- The x and y are 1-based column and row indices
isMarkedBy:: Int -> Int -> [Int] -> Int -> Bool
isMarkedBy x y board p 
     | (row x board) !! (y - 1) == p = True
     | otherwise = False

-- Returns the player of the stone placed on a place (x,y) of a board 
-- bd. The x and y are 1-based column and row indices
marker:: Int -> Int -> [Int] -> Int
marker x y board = (row x board) !! (y - 1)

-- Determines if all places of board bd marked, i.e., there is no empty place
isFull:: [Int] -> Bool
isFull (h:t)
     | h == 0 = False
     | t == [] = True
     | otherwise = isFull t
 
--Helper function
--Determines if a list has 5 consecutive p values
hasWinSeq:: [Int] -> Int -> Bool
hasWinSeq (h:t) p
     | (length t + 1 < 5) = False
     | (h == p) && (length t >= 4) = if (helper t p == True) then True else (hasWinSeq t p) 
     | otherwise = hasWinSeq t p 
     where helper t p = let rem = (take 4 t)
                    in (rem!!0 == p) && (rem!!1 == p) && (rem!!2 == p) && (rem!!3 == p)

-- Helper function that checks all rows of a board 
-- and determines if it one has a winning move
checkRows:: [Int] -> Int -> Int -> Bool
checkRows board r p
     | (r > size board) = False
     | (hasWinSeq (row r board) (p)) = True
     | otherwise = checkRows board (r + 1) p

-- Helper function that checks all columns of a board 
-- and determines if it one has a winning move
checkColumns:: [Int] -> Int -> Int -> Bool
checkColumns board c p
     | (c > size board) = False
     | (hasWinSeq (column c board) (p)) = True
     | otherwise = checkColumns board (c + 1) p

-- Helper function that checks all diagonals of a board starting at x y
-- and determines if it one has a winning move
checkDiagonals:: [Int] -> Int -> Int -> Int -> Bool
checkDiagonals board x y p 
     | (x > size board) = False
     | (y > size board) = checkDiagonals board (x + 1) (1) p
     | (hasWinSeq (rightDiagonal x y board) p) || (hasWinSeq (leftDiagonal x y board) p) = True
     | otherwise = checkDiagonals board (x) (y + 1) p      


-- Determines if the game played on a board bd is won by a player p. That is, does 
-- the board bd has a winning row for the player p?
isWonBy:: [Int] -> Int -> Bool
isWonBy board p 
     | (checkRows board 1 p) || (checkColumns board 1 p) || (checkDiagonals board 1 1 p) = True
     | otherwise = False

-- Determines if the game played on a board bd ended in a draw
isDraw:: [Int] -> Bool
isDraw board 
     | not (isFull board) = False 
     | otherwise = (not (isWonBy board mkPlayer)) && (not (isWonBy board mkOponent)) 

-- Determines if the game played on a board bd is over
isGameOver::[Int] -> Bool
isGameOver board
     | (isDraw board) = True
     | otherwise = (isWonBy board mkPlayer) || (isWonBy board mkOponent) 

-- Helper function that returns a string with the top row of the gameboard
-- " x 1 2 3 ... n\n"
getFirstRow:: [Int] -> Int -> String
getFirstRow board i
     | (i == size board) = " " ++ show (i `mod` 10) ++ "\n"
     | (i == 1) = " x" ++ " " ++ show (i `mod` 10) ++ getFirstRow board (i + 1)
     | otherwise = " " ++ show (i `mod` 10) ++ getFirstRow board (i + 1)

-- Helper function that returns a string with the top row of the gameboard
-- "y -----------\n"
getSecondRow:: [Int] -> Int -> String
getSecondRow board i
     | (i == size board) = "--\n"
     | (i == 1) = "y " ++ "--" ++ getSecondRow board (i + 1)
     | otherwise = "--" ++ getSecondRow board (i + 1)

--Helper function that takes an entire board and converts it into a String
boardToStrHelper::(Int -> Char) -> [Int] -> Int -> Int -> String
boardToStrHelper playerToChar board x y 
     | (x == size board) && (y == size board) = " " ++ [playerToChar ((row x board)!! (y - 1))] 
     | (y == size board) = " " ++ [playerToChar ((row x board)!! (y - 1))] ++ "\n" ++ boardToStrHelper playerToChar board (x + 1) 1 
     | (y == 1) = show (x `mod` 10) ++ "| " ++ [playerToChar ((row x board)!! (y - 1))] ++ boardToStrHelper playerToChar board x (y + 1) 
     | otherwise = " " ++ [playerToChar ((row x board)!! (y - 1))] ++ boardToStrHelper playerToChar board x (y + 1)

-- Return a string representation of a board bd. This is a
-- higher-order function, and playerToChar is a function that
-- converts a player to a character representation
boardToStr:: (Int -> Char) -> [Int] -> String
boardToStr playerToChar board = getFirstRow board 1 ++ getSecondRow board 1 ++ boardToStrHelper playerToChar board 1 1 




