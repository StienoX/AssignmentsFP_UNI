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
leaves (MkRose _ xs) = sum (map size xs)

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
hasWinner brd | isJust horzbrd = horzbrd
              | isJust diagbrd = diagbrd
              | isJust vertbrd = vertbrd
              | otherwise = Nothing

  where check :: (Row, Row, Row) -> Maybe Player
        check (a,b,c) | isJust(check' a) = check' a 
                      | isJust(check' b) = check' b
                      | isJust(check' c) = check' c
                      | otherwise = Nothing

            where check' :: (Field, Field, Field) -> Maybe Player
                  check' (X,X,X) = Just P1
                  check' (O,O,O) = Just P2
                  check' _       = Nothing 

        to3 :: (Row, Row) -> (Row,Row,Row)
        to3 (x,y) = (x,y,(B,B,B))
        diagbrd = check (to3 (diagonals brd))
        vertbrd = check (verticals brd)
        horzbrd = check brd
                           

-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree player brd | isJust(hasWinner brd) = MkRose brd [] --make leave
                    | otherwise = MkRose brd (map (gameTree (nextPlayer player)) (moves player brd))-- recursive moves

-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = undefined

-- | Minimax

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax = undefined

-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' = undefined

maximum' :: [Int] -> Int
maximum' = undefined

-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove = undefined

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