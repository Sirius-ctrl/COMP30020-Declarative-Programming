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

feedback [] [] = [0,0,0,0,0]
feedback target guess = [equalCheck target guess,0,0,0,0]

equalCheck :: [Card] -> [Card] -> Int

