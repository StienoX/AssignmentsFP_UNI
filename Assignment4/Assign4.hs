 
{-# language CPP #-}
{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

module Assignment4 where

#if __GLASGOW_HASKELL__ >= 804
import Prelude hiding (Monoid, mempty, foldMap, Foldable, (<>))
#elif __GLASGOW_HASKELL__ >= 710
import Prelude hiding (Monoid, mempty, foldMap, Foldable)
#endif

import Data.List     (foldl', group, sort)
import Data.Set      (Set, empty, insert)

-- | Containers

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

-- * Exercise 1

instance Functor Rose where
    fmap f (MkRose x []) = MkRose (f x) []
    fmap f (MkRose x xs) = MkRose (f x) (map (fmap f) xs)

class Monoid a where
    mempty ::           a
    (<>)   :: a -> a -> a

instance Monoid [a] where
    mempty = []
    (<>)   = (++)

newtype Sum     a = Sum     { unSum     :: a } deriving (Eq, Show)
newtype Product a = Product { unProduct :: a } deriving (Eq, Show)

instance Num a => Monoid (Sum a) where
    mempty           = Sum 0
    Sum n1 <> Sum n2 = Sum (n1 + n2)

-- * Exercise 2

instance Num a => Monoid (Product a) where
    mempty                     = Product 1
    Product n1 <> Product n2   = Product (n1 * n2)

class Functor f => Foldable f where
    fold    :: Monoid m =>             f m -> m
    foldMap :: Monoid m => (a -> m) -> f a -> m
    -- * Exercise 4
    foldMap f xs = fold (fmap f xs)

instance Foldable [] where
    fold = foldr (<>) mempty

-- * Exercise 3

instance Foldable Rose where
    fold (MkRose x []) = x <> mempty
    fold (MkRose x xs) = foldr (<>) x (map fold xs)

-- * Exercise 5

fsum, fproduct :: (Foldable f, Num a) => f a -> a
fsum     x = unSum (foldMap Sum x)
fproduct x = unProduct (foldMap Product x) 

-- | Poker

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
    deriving (Bounded, Enum, Eq, Ord)

-- * Exercise 6

instance Show Rank where
    show R2   = "2"
    show R3   = "3"
    show R4   = "4"
    show R5   = "5"
    show R6   = "6"
    show R7   = "7"
    show R8   = "8"
    show R9   = "9"
    show R10  = "10"
    show J    = "J"
    show Q    = "Q"
    show K    = "K"
    show A    = "A"

data Suit = S | H | D | C
    deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

-- * Exercise 7

instance Show Card where
    show (Card { rank = r, suit = s }) = show r ++ show s

type Deck = [Card]

-- * Exercise 8

mkDeck :: [Rank] -> [Suit] -> [Card]
mkDeck xs ys= [Card {rank = x, suit = y} | x <- xs, y <- ys]

fullDeck, piquetDeck :: Deck
fullDeck   = mkDeck [R2 ..] [S ..C]
piquetDeck = mkDeck [R7 ..] [S ..C]

newtype Hand = Hand { unHand :: [Card] } deriving (Eq, Show)

data HandCategory
    = HighCard      [Rank]
    | OnePair       Rank [Rank]
    | TwoPair       Rank Rank Rank
    | ThreeOfAKind  Rank Rank Rank
    | Straight      Rank
    | Flush         [Rank]
    | FullHouse     Rank Rank
    | FourOfAKind   Rank Rank
    | StraightFlush Rank
    deriving (Eq, Ord, Show)
    
-- * Exercise 9
    
sameSuits :: Hand -> Bool
sameSuits Hand {unHand = []} = False
sameSuits Hand {unHand = card:cards} = all (==card) cards

-- * Exercise 10

isStraight :: [Rank] -> Maybe Rank
isStraight [] = Nothing
isStraight (x:xs) = isStraight' xs (x,0)
  where isStraight' :: [Rank] -> (Rank,Int) -> Maybe Rank
        isStraight' _ (y,4) = (Just y)
        isStraight' [] _ = Nothing
        isStraight' (R2:xs) (A,n) = isStraight' xs (R2,n+1)
        isStraight' (R2:xs) (_,n) = isStraight' xs (R2,0)
        isStraight' (x:xs) (y,n) | x == (succ y) = isStraight' xs (x,n+1)
                                 | otherwise = isStraight' xs (x,0)


-- * Exercise 11

ranks :: Hand -> [Rank]
ranks Hand {unHand = cards} = reverse (sort (map rank cards))

-- * Exercise 12

order :: Hand -> [(Int, Rank)]
order h = (map rank (unHand h))
  where getRank (_,x) = x
        

-- * Exercise 13

handCategory :: Hand -> HandCategory
handCategory = undefined

-- * Exercise 14

instance Ord Hand where
    compare = undefined

-- * Exercise 15

combs :: Int -> [a] -> [[a]]
combs = undefined

-- * Exercise 16

allHands :: Deck -> [Hand]
allHands = undefined

-- * Exercise 17

distinctHands :: Deck -> Set Hand
distinctHands = undefined

-- * Question 1

{- ANSWER -}
