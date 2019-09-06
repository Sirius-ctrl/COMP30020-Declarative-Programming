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
nextGuess (previous, (process, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | (process == 4) && (exact /= lastExact)
        = guessCards (previous, (5, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    -- | process == 5
    --     = ([], (process, exact, snum, previous, ror, lenAll, drop 1 candidates))
    | process < 3
        = guessRank (previous, (process, exact, snum, previous, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    -- | process > 3
    --     = ([ceiling, floor], (process, exact, snum, previous, ror, lenAll, drop 1 candidates))
    | process > 3
        = (head candidates, (4, exact, snum, previous, ror, lenAll, drop 1 candidates))
    | otherwise
        = nextGuess (previous, (4, exact, snum, previous, ror, lenAll, suitPatternCan)) (exact,lower,sameR,higher,sameS)
    where
          suitPatternCan = filter (\x -> suitMatcher x snum) fixedRangeCan
          fixedRangeCan  = myCardFilter upperBoundCan (map $ \x -> rank x == rank floor) or
          -- rank x == rank floor
          -- filter out the candidates which do not contains the upper bound rank
          upperBoundCan  = myCardFilter primaryCan (map $ \x -> rank x == rank ceiling) or
          -- rank x == rank ceiling
          -- generate all possible candidates based on the reducedDeck
          primaryCan     = generateCandidates (length previous) reducedDeck []
          -- remove all cards that contains card outside of the range
          reducedDeck    = filter (\x -> rank x >= (rank $ floor)) $ filter (\x -> (rank x) <= (rank $ ceiling)) allCards
          floor          = minRank previous
          ceiling        = maxRank previous


-- | this function can gradually explore the range of the answers
guessRank :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessRank (previous, (process, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    -- | process == 0
    --     = ([Card Club R2], (process, exact, snum, previous, ror, lenAll, []))        
    -- we have some cards in our guess out side of the range at both upper and lower boundary
    | l && h && (length snum >= 3)
        = guessEachSuit (expand, (process, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    -- we have cards sit outside of the lower boundary
    | l && (length snum >= 3)
        = guessEachSuit (lowerMin, (process, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    --  we have cards sit outside of the upper boundary
    | h && (length snum >= 3)
        = guessEachSuit (higherMax, (process, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    -- | notAll && (length snum >= 3) && (length previous /= 2)
    --     = guessEachSuit (shrink, (process, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    -- | process == 0
    --     = ([Card Club R2], (process, exact, snum, previous, ror, lenAll, []))
    | length snum < 3
        = guessEachSuit (previous, (process, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | otherwise
        = guessEachSuit (previous, (2, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    where l = lower /= 0
          h = higher /= 0
          notAll = sameR < 1
          -- shrink = improveRank (improveRank avo minRank succ) maxRank pred
          avo = colliAvo previous 0 allSuits
          highn = maxnRank avo higher
          lown = minnRank avo lower

          expand = improveRank (improveRank avo minRank pred) maxRank succ
          lowerMin = (improveRank lown (head) pred) ++ (avo \\ lown)
          higherMax = (improveRank highn (head) succ) ++ (avo \\ highn)


colliAvo :: [Card] -> Int -> [Suit] -> [Card]
colliAvo [] _ _ = []
colliAvo [x] n slist = [changeSuit (slist !! n) x]
colliAvo (x:xs) n slist = (colliAvo [x] n slist) ++ (colliAvo xs (mod (n+1) 4) slist)


-- | guess number card in answer for each suit
guessEachSuit :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessEachSuit (nextToGuess, (process, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | (length snum == 3)
        = guessEachSuit (nextToGuess, (process, lastExact, snum ++ [sameS], beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    | (length snum < 3)
        = (shiftedCard, (process, lastExact, snum ++ [sameS], beforePre, ror, lenAll, candidates))
    | (process == 2) && (length snum == 4)
        = (nextToGuess, (3, lastExact, snum, beforePre, ror, lenAll, candidates))
    | otherwise
        = (nextToGuess, (0, lastExact, snum, beforePre, ror, lenAll, candidates))
    where shiftedCard = map (\x -> changeSuit (succ $ suit x) x) nextToGuess


-- | this function will make use the feedback and gradually shrink the size of candidates
guessCards :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessCards (previous, (process, lastExact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
    -- | True = ([], (process, exact, snum, beforePre, ror, lenAll, newCan))
    | diffv == length newC = nextGuess (previous, (process, exact, snum, beforePre, ror, lenAll, newCan)) (exact,lower,sameR,higher,sameS)
    | otherwise            = nextGuess (previous, (process, exact, snum, beforePre, ror, lenAll, candidates)) (exact,lower,sameR,higher,sameS)
        where newCan    = filter (\x -> containsOne possible x) candidates
              possible  = generateCandidates diffv newC []
              diffv     = abs $ exact-lastExact
              newC      = previous \\ beforePre


containsOne :: [[Card]] -> [Card] -> Bool
containsOne [] _ = True
containsOne [x] cards = allIn x cards
containsOne (x:xs) cards = allIn x cards || containsOne xs cards


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


-- | get the card with maximum rank
maxRank :: [Card] -> Card
maxRank [x] = x
maxRank (x:xs)
    | rank x > (rank $ maxRank xs)  = x
    | otherwise                     = maxRank xs


-- | get the card with minimum rank
minRank :: [Card] -> Card
minRank [x] = x
minRank (x:xs)
    | rank x < (rank $ minRank xs)  = x
    | otherwise                     = minRank xs


maxnRank :: [Card] -> Int -> [Card]
maxnRank cards 0 = []
maxnRank cards n = [mr] ++ (maxnRank remains (n-1))
    where mr = maxRank cards
          remains = delete mr cards


minnRank :: [Card] -> Int -> [Card]
minnRank cards 0 = []
minnRank cards n = [mr] ++ (minnRank remains (n-1))
    where mr = minRank cards
          remains = delete mr cards


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
generateCandidates n seed candidates = generateCandidates (n-1) seed (nub $ map sort [x++[y]| x <- candidates, y <- seed, notElem y x])


-- | this represent all the card in this game
-- ANCHOR
allCards :: [Card]
allCards = enumFromTo (toEnum 0::Card) (toEnum 51::Card)

allSuits :: [Suit]
allSuits = [suit $ Card Club R2, suit $ Card Heart R2,suit $ Card Diamond R2,suit $ Card Spade R2]

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
--   * [Int], length of above list
--   * [[Card]], all candidates
type GameState = (Int, Int, [Int], [Card], [Rankor], [Int], [[Card]])

data Rankor = Rankor Rank | Rankers [Card]
             deriving (Show)


-- |takes the number of cards in the answer as input and returns 
--  a pair of an initial guess, which should be a list of the specified number of cards, 
--  and a game state. The number of cards specified will be 2 
--  for most of the test, and 3 or 4 for the remaining tests
initialGuess :: Int -> ([Card],GameState)
initialGuess n = ((getCards n []), (1,0,[],[],[],[0,0,0,0,0,0],[]))

-- |take n adjacent cards for initial guessing
getCards :: Int -> [Card] -> [Card]
getCards 0 cardList = cardList
getCards n cardList
    | length cardList == 0  = getCards (n-1) [Card Club R6]
    | otherwise             = getCards (n-1) $ [succ $ head cardList] ++ cardList
