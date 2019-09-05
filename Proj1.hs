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


-- | A generic match function to match card with same suit or rank
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
nextGuess (previous, (process, lastExact, snum, beforePre, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | (lastExact /= exact) && (length candidates > 0)  = guessCards (previous, (process, lastExact, snum, beforePre, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | process <= 2                  = guessRank (previous, (1, exact, snum, previous, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | length candidates /= 0        = (head candidates, (process, exact, snum, previous, cor, ror, lenAll, drop 1 candidates))
    | otherwise                     = nextGuess (previous, (process, exact, snum, previous, cor, ror, lenAll, fixedRangeCan)) (exact,lower,sameR,higher,sameS)
    where suitPatternCan = filter (\x -> suitMatcher x snum) fixedRangeCan
          fixedRangeCan  = myCardFilter upperBoundCan (map $ \x -> rank x == rank floor) or
          -- filter out the candidates which do not contains the upper bound rank
          upperBoundCan  = myCardFilter primaryCan (map $ \x -> rank x == rank ceiling) or
          -- generate all possible candidates based on the reducedDeck
          primaryCan     = generateCandidates (length previous) reducedDeck []
          -- remove all cards that contains card outside of the range
          reducedDeck    = filter (\x -> rank x >= (rank $ floor)) $ filter (\x -> (rank x) <= (rank $ ceiling)) allCards
          floor          = minimum previous
          ceiling        = maximum previous


-- | this function can gradually explore the range of the answers
guessRank :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessRank (previous, (process, lastExact, snum, beforePre, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | l && h        = guessEachSuit (improveRank (improveRank previous minimum pred) maximum succ, (process, lastExact, snum, beforePre, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | l             = guessEachSuit (improveRank previous minimum pred, (process, lastExact, snum, beforePre, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | h             = guessEachSuit (improveRank previous maximum succ, (process, lastExact, snum, beforePre, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | otherwise     = guessEachSuit (previous, (2, lastExact, snum, beforePre, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    where l = lower /= 0
          h = higher /= 0


-- | guess number card in answer for each suit
guessEachSuit :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessEachSuit (nextToGuess, (process, lastExact, snum, beforePre, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | length snum == 3              = (nextToGuess, (process, lastExact, snum ++ [sameS], beforePre, cor, ror, lenAll, candidates))
    | (length snum < 3)             = (shiftedCard, (process, lastExact, snum ++ [sameS], beforePre, cor, ror, lenAll, candidates))
    | process == 2                  = (nextToGuess, (3,lastExact, snum, beforePre, cor, ror, lenAll, candidates))
    | otherwise                     = (nextToGuess, (process, lastExact, snum, beforePre, cor, ror, lenAll, candidates))
    where shiftedCard = map (\x -> changeSuit (succ $ suit x) x) nextToGuess


-- | this function will make use the feedback and gradually shrink the size of candidates
guessCards :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessCards (previous, (process, lastExact, snum, beforePre, cor, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    -- NOTE: seems like the cor is useless in this case
    | length diff == (abs $ exact-lastExact)   = nextGuess (previous, (process, exact, snum, beforePre, newcor, ror, lenAll, newCan1)) (exact,lower,sameR,higher,sameS)
    | otherwise                              = nextGuess (previous, (process, exact, snum, beforePre, newcor, ror, lenAll, newCan2)) (exact,lower,sameR,higher,sameS)
    where diff = [x| x <- previous, notElem x beforePre]
          newCan1 = filter (\x -> allIn diff x) candidates
          newCan2 = filter (\x -> oneIn diff x) candidates
          newcor = cor ++ [Cardors diff]


-- | test whether all elements in first argument are in second argument
allIn :: [Card] -> [Card] -> Bool
allIn [] b = True
allIn [x] b = elem x b
allIn (x:xs) b = (elem x b) && (allIn xs b)


-- | test whether at least one elements in first argument are in second argument
oneIn :: [Card] -> [Card] -> Bool
oneIn [] b = True
oneIn [x] b = elem x b
oneIn (x:xs) b = (elem x b) || (oneIn xs b)


-- | expand the range of the guess card where
--   * f1 defined whether we like the expend the lower boundary of higher bound, since when comparing rank
--     they should have the same suit. NOTE: Therefore, simply using maximum and minimum is good enough.
--   * f2 defined how we could expand our boundary
improveRank :: [Card] -> ([Card] -> Card) -> (Card -> Card) -> [Card]
improveRank [] _ _ = []
improveRank cards f1 f2 = ([f2 target]) ++ (delete target cards)
    where target = f1 cards


-- | generate candidates based on a subset of allCards which is seed
generateCandidates :: Int -> [Card] -> [[Card]] -> [[Card]]
generateCandidates 0 _ candidates = candidates
generateCandidates n seed [] = generateCandidates (n-1) seed [[x]| x <- seed]
generateCandidates n seed candidates = generateCandidates (n-1) seed [x++[y]| x <- candidates, y <- seed, notElem y x]


-- | this represent all the card in this game
-- ANCHOR
allCards :: [Card]
allCards = enumFromTo (toEnum 0::Card) (toEnum 51::Card)


-- | change the suit of the card to given suit
-- ANCHOR
changeSuit :: Suit -> Card -> Card
changeSuit news (Card s r) = Card news r


-- | the function will examine whether the given candidates can match the patter of the given suit
--   pattern which are represented in the same form as in GameState
--   Club Diamond Heart Spade from head to tail
-- ANCHOR
suitMatcher :: [Card] -> [Int] -> Bool
suitMatcher cards pattern
    |ress == pattern    = True
    | otherwise         = False
    where resc = [length [x|x <- cards, suit x == (suit $ Card Club R2)]]
          resd = resc ++ [length [x|x <- cards, suit x == (suit $ Card Diamond R2)]]
          resh = resd ++ [length [x|x <- cards, suit x == (suit $ Card Heart R2)]]
          ress = resh ++ [length [x|x <- cards, suit x == (suit $ Card Spade R2)]]


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


-- | * Int, represent which process it go through can only be a number in [0..4]
--   * Int, previous number of exact
--   * [Int], represent number of card for each suit, it contains 4 Int, they are representing Club Diamond Heart Spade from left to right
--   * [Card], this represent the card before the the one that we provided with feedback
--   * [Rankor] the rank pair which either of them will be in the card (only useful when there are 4 cards to guess)
--   * [Cardor] the card pair which either of them will be in the card (only useful when there are 4 cards to guess)
--   * [Int], length of above 2 list
--   * [[Card]], all candidates
type GameState = (Int, Int, [Int], [Card], [Cardor], [Rankor], [Int], [[Card]])

data Rankor = Rankor Rank | Rankers [Card]
             deriving (Show)
data Cardor = Cardor Card | Cardors [Card]
            deriving (Show)


-- |takes the number of cards in the answer as input and returns 
--  a pair of an initial guess, which should be a list of the specified number of cards, 
--  and a game state. The number of cards specified will be 2 
--  for most of the test, and 3 or 4 for the remaining tests
initialGuess :: Int -> ([Card],GameState)
initialGuess n = ((getCards n []), (1,0,[],[],[],[],[0,0,0,0,0,0],[]))

-- |take n adjacent cards for initial guessing
getCards :: Int -> [Card] -> [Card]
getCards 0 cardList = cardList
getCards n cardList
    | length cardList == 0  = getCards (n-1) [Card Club R5]
    | otherwise             = getCards (n-1) $ [succ $ head cardList] ++ cardList
