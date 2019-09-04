module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Data.List
import Card


-- |takes a target and a guess (in that order), 
--  each represented as a list of Cards, and returns the five feedback numbers, 
--  as explained above, as a tuple.
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)

feedback [] [] = (0,0,0,0,0)
feedback target guess = (exact,lower,sameR,higher,sameS)
    where exact = correctCards target guess
          lower = rankComp target guess minimum (<)
          sameR = oneMatch (picker rank target) (picker rank guess)
          higher = rankComp target guess maximum (>)
          sameS = oneMatch (picker suit target) (picker suit guess)


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


oneMatch :: Eq a => [a] -> [a] -> Int
oneMatch [] _ = 0
oneMatch target guess
    | elem next guess  = 1 + oneMatch (drop 1 target) (delete next guess)
    | otherwise        = oneMatch (drop 1 target) guess
    where next = head target

-- | takes as input a pair of the previous guess and game state, and the feedback 
-- to this guess as a quintuple of counts of correct cards, low ranks, correct 
-- ranks, high ranks, and correct suits, and returns a pair of the next guess
-- and new game state.
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (previous, (process, snum, cor, sor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    -- | length ror > 0                = ([], (process, snum, cor, sor, ror, lenAll, candidates))
    | process == 1                  = guessRank (previous, (1, snum, cor, sor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | length candidates /= 0        = (head candidates, (process, snum, cor, sor, ror, lenAll, drop 1 candidates))
    | otherwise                     = nextGuess (previous, (process, snum, cor, sor, ror, lenAll, fixedRangeCan)) (exact,lower,sameR,higher,sameS)
    where fixedRangeCan = myCardFilter upperBoundCan (map $ \x -> rank x == rank floor) or
          -- filter out the candidates which do not contains the upper bound rank
          upperBoundCan = myCardFilter primaryCan (map $ \x -> rank x == rank ceiling) or
          -- generate all possible candidates based on the reducedDeck
          primaryCan = generateCandidates (length previous) reducedDeck []
          -- remove all cards that contains card outside of the range
          reducedDeck = filter (\x -> rank x >= (rank $ floor)) $ filter (\x -> (rank x) <= (rank $ ceiling)) allCards
          floor = minimum previous
          ceiling = maximum previous


guessRank :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessRank (previous, (process, snum, cor, sor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | l && h    = (improveRank (improveRank previous minimum pred) maximum succ, (process,[0,0,0,0],[],[],[],[0,0,0,0,0,0],[]))
    | l         = (improveRank previous minimum pred, (process,[0,0,0,0],[],[], newh,[0,0,0,0,0,0],[]))
    | h         = (improveRank previous maximum succ, (process,[0,0,0,0],[],[], newl,[0,0,0,0,0,0],[]))
    | otherwise = (previous, (2,[0,0,0,0],[],[], newror,[0,0,0,0,0,0],[]))
    where l = lower /= 0
          h = higher /= 0
          newror = newh ++ newl
          newh   = [Rankor (Just (rank $ maximum previous)) Nothing]
          newl   = [Rankor (Just (rank $ minimum previous)) Nothing]


-- | expand the range of the guess card where
--   * f1 defined whether we like the expend the lower boundary of higher bound, since when comparing rank
--     they should have the same suit. NOTE: Therefore, simply using maximum and minimum is good enough.
--   * f2 defined how we could expand our boundary
improveRank :: [Card] -> ([Card] -> Card) -> (Card -> Card) -> [Card]
improveRank [] _ _ = []
improveRank cards f1 f2 = ([f2 target]) ++ (delete target cards)
    where target = f1 cards

generateCandidates :: Int -> [Card] -> [[Card]] -> [[Card]]
generateCandidates 0 _ candidates = candidates
generateCandidates n seed [] = generateCandidates (n-1) seed [[x]| x <- seed]
generateCandidates n seed candidates = generateCandidates (n-1) seed [x++[y]| x <- candidates, y <- seed, notElem y x]


-- | this represent all the card in this game
allCards :: [Card]
allCards = enumFromTo (toEnum 0::Card) (toEnum 51::Card)


-- | this function could go through the whole candidate list and remove those which does not
--   satisfy the condition. This function are mostly used for shrinking the candidate space
--   based on the feedback.
--   * [x] or (x:xs) are the card list that we want to filter out
--   * f1 is the function that take a single candidate set (2,3 or 4 cards) and examine each card to see if they met 
--     the condition, it should be a curried "map" function for all the time.
--   * f2 could only be either "and" or "or" which could summarize the results of the f1
myCardFilter :: [[Card]] -> ([Card] -> [Bool]) -> ([Bool] -> Bool) -> [[Card]]
myCardFilter [] _ _ = []
myCardFilter [x] f1 f2
    | f2 $ f1 x = [x]
    | otherwise = []
myCardFilter (x:xs) f1 f2
    | f2 $ f1 x = [x] ++ myCardFilter xs f1 f2
    | otherwise = myCardFilter xs f1 f2


-- | * Int, represent which process it go through can only be a number in [1..4]
--   * [Int], represent number of card for each suit, it contains 4 Int, they are representing Club Diamond Heart Spade from left to right
--   * [Suitor] the suit pair which either of them will be in the card (only useful when there are 4 cards to guess)
--   * [Rankor] the rank pair which either of them will be in the card (only useful when there are 4 cards to guess)
--   * [Cardor] the card pair which either of them will be in the card (only useful when there are 4 cards to guess)
--   * [Int], length of above 3 list
--   * [[Card]], all candidates
type GameState = (Int, [Int], [Cardor], [Suitor], [Rankor], [Int], [[Card]])

data Suitor = Suitor (Maybe Suit) (Maybe Suit) deriving (Show)
data Rankor = Rankor (Maybe Rank) (Maybe Rank) deriving (Show)
data Cardor = Cardor (Maybe Card) (Maybe Card) deriving (Show)


-- |takes the number of cards in the answer as input and returns 
--  a pair of an initial guess, which should be a list of the specified number of cards, 
--  and a game state. The number of cards specified will be 2 
--  for most of the test, and 3 or 4 for the remaining tests
initialGuess :: Int -> ([Card],GameState)
initialGuess n = ((getCards n []), (1,[0,0,0,0],[],[],[],[0,0,0,0,0,0],[]))

-- |take n adjacent cards for initial guessing
getCards :: Int -> [Card] -> [Card]
getCards 0 cardList = cardList
getCards n cardList
    | length cardList == 0  = getCards (n-1) [Card Club R5]
    | otherwise             = getCards (n-1) $ [succ $ head cardList] ++ cardList
