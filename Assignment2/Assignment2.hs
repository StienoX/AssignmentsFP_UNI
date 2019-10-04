{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

 {-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:

 http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
                      -- Don't forget to rename it back when submitting!

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = MkRose a [Rose a]
 deriving (Eq, Show)

-- Exercise 1

root :: Rose a -> a
root (MkRose a _) = a

children :: Rose a -> [Rose a]
children (MkRose _ xs) = xs

-- Exercise 2

size :: Rose a -> Int
size (MkRose _ []) = 1
size (MkRose _ xs) = 1 + sum (map size xs)

leaves :: Rose a -> Int
leaves (MkRose _ []) = 1
leaves (MkRose _ xs) = sum (map leaves xs)

-- | State representation

-- * Players

data Player = P1 | P2
 deriving (Eq, Ord)

instance Show Player where
 show P1 = "Player 1"
 show P2 = "Player 2"
 
-- Exercise 3
 
nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- * Board

data Field = X | O | B
 deriving (Eq, Ord)

instance Show Field where
 show X = "X"
 show O = "O"
 show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O


type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals ((a0,a1,a2),(b0,b1,b2),(c0,c1,c2)) = ((a0,b0,c0),(a1,b1,c1),(a2,b2,c2)) --Transposes Board

diagonals :: Board -> (Row, Row)
diagonals ((a0,_,a2),(_,b1,_),(c0,_,c2)) = ((a0,b1,c2),(a2,b1,c0))

-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B,B,B),(B,B,B),(B,B,B))

-- Exercise 7

printBoard :: Board -> String
printBoard (a,b,c) = mk a ++ ln ++ mk b ++ ln ++ mk c
  where mk (a,b,c) = show a ++ "|" ++ show b ++ "|" ++ show c ++ "\n"
        ln = "-+-+-\n"

-- | Move generation
convert :: Board -> [Field]
convert ((a,b,c),(d,e,f),(g,h,i)) = [a,b,c,d,e,f,g,h,i]          
-- Exercise 8
          
moves :: Player -> Board -> [Board]
moves player brd = mapMaybe check (brds brd)
  where brds ((a,b,c),(d,e,f),(g,h,i)) = [
             ((s,b,c),(d,e,f),(g,h,i)),
             ((a,s,c),(d,e,f),(g,h,i)),
             ((a,b,s),(d,e,f),(g,h,i)),
             ((a,b,c),(s,e,f),(g,h,i)),
             ((a,b,c),(d,s,f),(g,h,i)),
             ((a,b,c),(d,e,s),(g,h,i)),
             ((a,b,c),(d,e,f),(s,h,i)),
             ((a,b,c),(d,e,f),(g,s,i)),
             ((a,b,c),(d,e,f),(g,h,s))]
        s = symbol player
        count :: Board -> Int
        count lbrd = length (filter (==B) (convert lbrd))
        c_b = count brd
        check :: Board -> Maybe Board 
        check nbrd | c_b > count nbrd = Just nbrd
                   | otherwise = Nothing


-- | Gametree generation

-- Exercise 9

hasWinner :: Board -> Maybe Player
hasWinner brd = getWinner (concat [diagbrd, vertbrd, horzbrd])
  where check :: (Row, Row, Row) -> [Player]
        check (a,b,c) = mapMaybe check' [a,b,c]

        checkDiag :: (Row, Row) -> [Player]
        checkDiag (a,b) = mapMaybe check' [a,b]

        check' :: (Field, Field, Field) -> Maybe Player
        check' (X,X,X) = Just P1
        check' (O,O,O) = Just P2
        check' _       = Nothing 

        diagbrd = checkDiag (diagonals brd)
        vertbrd = check (verticals brd)
        horzbrd = check brd

        getWinner :: [Player] -> Maybe Player
        getWinner [] = Nothing
        getWinner (x:_) = Just x
-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree player brd = case hasWinner brd of
    Nothing -> MkRose brd (map (gameTree (nextPlayer player)) (moves player brd))-- recursive moves
    Just _  -> MkRose brd [] --make leave

-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = leaves (gameTree P1 emptyBoard)
-- | Minimax

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax player x = minimax' player x
  where minimax' :: Player -> Rose Board -> Rose Int
        minimax' curPlayer (MkRose brd []) = MkRose (getValue player (hasWinner brd)) []
        minimax' curPlayer (MkRose _ brds) | player == curPlayer = generateRose maximum'
                                           | otherwise = generateRose minimum'
            where generateRose f = MkRose (f (map root (getIntRoses curPlayer brds))) (getIntRoses curPlayer brds)
                  getIntRoses :: Player -> [Rose Board] -> [Rose Int]
                  getIntRoses curPlayer = map (minimax' (nextPlayer curPlayer))

        getValue :: Player -> Maybe Player -> Int 
        getValue player winner = case winner of
            Nothing -> 0
            Just winner -> match winner player
        match :: Player -> Player -> Int
        match winner player | winner == player = 1
                            | otherwise = -1


-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' (-1:_) = -1
minimum' (x:y:xs) | x <= y    = minimum' (x:xs)
                  | otherwise = minimum' (y:xs)
minimum'  [x]     = x
minimum' [] = 0
maximum' :: [Int] -> Int
maximum' (1:_) = 1
maximum' (x:y:xs) | x >= y    = maximum' (x:xs)
                  | otherwise = maximum' (y:xs)
maximum' [x]      = x
maximum' [] = 0
                  

-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove player brd = getOptimalMove avaibleMoves
    where avaibleMoves :: [Rose Board] 
          avaibleMoves = children (gameTree player brd)
          combine :: Rose Board -> (Board, Rose Int)
          combine move = (root move, minimax (nextPlayer player) move)
          getOptimalMove :: [Rose Board] -> Maybe Board
          getOptimalMove [] = Nothing
          getOptimalMove (x:xs) = Just (first (foldl compare' (combine x) (map combine xs)))
            where compare' :: (Board, Rose Int) -> (Board, Rose Int) -> (Board, Rose Int)
                  compare' (a,b) (c,d) | root b <= root d = (a,b)
                                       | otherwise = (c,d)
                  first (x,_) = x

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
 show Human    = "H"
 show Computer = "C"

main :: IO ()
main = do
 typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                    [Human, Computer]
 typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                    [Human, Computer]

 let playerType :: Player -> PlayerType 
     playerType P1 = typeOfP1
     playerType P2 = typeOfP2

     gameLoop :: Player -> Board -> IO ()
     gameLoop p b = do
         putStrLn ("\n" ++ printBoard b)
         case hasWinner b of
             Just p  -> putStrLn (show p ++ " has won!")
             Nothing -> do
                 putStr   ("It's " ++ show p ++ "'s turn. ")
                 mb' <- case playerType p of
                     Human    -> humanMove    p b
                     Computer -> computerMove p b
                 case mb' of
                     Nothing -> do putStr   "No more moves are possible. "
                                   putStrLn "It's a draw."
                     Just b' -> gameLoop (nextPlayer p) b'

     humanMove :: Player -> Board -> IO (Maybe Board)
     humanMove p b = do
         case moves p b of
           [] -> return Nothing
           possibleMoves -> do
             putStrLn "Possible moves are:"
             putStrLn (listMoves possibleMoves)
             i <- askFor "Make your choice:" [1..length possibleMoves]
             return (Just (possibleMoves !! (i-1)))

     computerMove :: Player -> Board -> IO (Maybe Board)
     computerMove p b = do
         putStrLn "Thinking..."
         return (makeMove p b)

     listMoves :: [Board] -> String
     listMoves = intercalate "\n"
                 . map (intercalate "    ")
                 . transpose
                 . map lines
                 . map (\(i,b) -> "(" ++ show i ++ "): \n" ++ printBoard b) 
                 . zip [1 :: Integer ..]

 gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
 putStr (m ++ " ")
 hFlush stdout
 i <- getLine
 case find ((map toLower i ==) . map toLower . show) xs of
     Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                              ++ intercalate ", " (map show xs) ++ "."
                   askFor m xs
     Just y  -> return y