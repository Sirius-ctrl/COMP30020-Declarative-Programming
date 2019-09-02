module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Data.List
import Card

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
-- takes as input a pair of the previous guess and game state, and the feedback 
-- to this guess as a quintuple of counts of correct cards, low ranks, correct 
-- ranks, high ranks, and correct suits, and returns a pair of the next guess 
-- and new game state.

type GameState = (Int, Int, Int, Int, Int)


-- |takes the number of cards in the answer as input and returns 
--  a pair of an initial guess, which should be a list of the specified number of cards, 
--  and a game state. The number of cards specified will be 2 
--  for most of the test, and 3 or 4 for the remaining tests
initialGuess :: Int -> ([Card],GameState)
initialGuess n = ((getCards n []), (0,0,0,0,0))

-- |take n adjacent cards for initial guessing
getCards :: Int -> [Card] -> [Card]
getCards 0 cardList = cardList
getCards n cardList
    | length cardList == 0  = getCards (n-1) [Card Club R5]
    | otherwise             = getCards (n-1) $ [succ $ head cardList] ++ cardList



-- |takes a target and a guess (in that order), 
--  each represented as a list of Cards, and returns the five feedback numbers, 
--  as explained above, as a tuple.
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)

feedback [] [] = (0,0,0,0,0)
feedback target guess = (exact,lower,sameR,higher,sameS)
    where exact = correctCards target guess
          lower = rankComp target guess minimum (<)
          sameR = correctRank (picker rank target) (picker rank guess)
          higher = rankComp target guess maximum (>)
          sameS = correctSuit (picker suit target) (picker suit guess)


-- | pick part of the card info, where the f could only be either (rank or suit) which
--   defined in Card.hs to take either rank or suit of the card.
picker :: (Card -> a) -> [Card] -> [a]
picker f []  = []
picker f [x] = [f x]
picker f (x:xs) = [f x] ++ (picker f xs)


-- | Used for rule 2 and 4 in feedback, it could use customized binary operations
--   which made this function more flexible to handle both of the case
--   where f1 could only be either (minimum or maximum) and f2 is the binary operator which
--   could only be either (< or >).
rankComp :: [Card] -> [Card] -> ([Rank] -> Rank) -> (Rank -> Rank -> Bool) -> Int
rankComp target guess f1 f2 = length [x| x <- ranks, f2 x $ f1 ranksG]
    where ranksG = picker rank guess
          ranks = picker rank target


-- | compare whether there is a matched card in each set of input
--   In this case, it compare the targets and guesses
correctCards :: [Card] -> [Card] -> Int
correctCards _ [] = 0
correctCards target guess = length [x |x <- guess, y <- target, x == y]


-- oneMatch :: [a] -> [a] -> Int
-- oneMatch [] _ = 0
-- oneMatch target guess
--     | elem next guess  = 1 + correctRank (drop 1 target) (delete next guess)
--     | otherwise        = correctRank (drop 1 target) guess
--     where next = head target

correctRank :: [Rank] -> [Rank] -> Int
correctRank [] _ = 0
correctRank target guess
    | elem next guess  = 1 + correctRank (drop 1 target) (delete next guess)
    | otherwise        = correctRank (drop 1 target) guess
    where next = head target

correctSuit :: [Suit] -> [Suit] -> Int
correctSuit [] _ = 0
correctSuit target guess
    | elem next guess  = 1 + correctSuit (drop 1 target) (delete next guess)
    | otherwise        = correctSuit (drop 1 target) guess
    where next = head target

