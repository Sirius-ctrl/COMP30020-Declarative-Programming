--  File     : Proj1.hs
--  Author   : Xinyao Niu
--  userName : xinyaon
--  studentID: 900721
--  Purpose  : main program for COMP30020 project1


module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Data.List
import Card


-- |takes a target and a guess (in that order), 
--  each represented as a list of Cards, and returns the five feedback numbers, 
--  as a tuple.
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)

feedback [] [] = (0,0,0,0,0)
feedback target guess = (exact,lower,sameR,higher,sameS)
    where exact = correctCards target guess
          lower = rankComp target guess minimum (<)
          sameR = oneMatch (map rank target) (map rank guess)
          higher = rankComp target guess maximum (>)
          sameS = oneMatch (map suit target) (map suit guess)


-- | Used for rule 2 and 4 in feedback, it could use customized binary operations
--   which made this function more flexible to handle both cases
--   where f1 could only be either (minimum or maximum) and f2 is the binary operator which
--   could only be either (< or >).
rankComp :: [Card] -> [Card] -> ([Rank] -> Rank) -> (Rank -> Rank -> Bool) -> Int
rankComp target guess f1 f2 = length [x| x <- ranks, f2 x $ f1 ranksG]
    where ranksG = map rank guess
          ranks = map rank target


-- | compare whether there is a matched card in each set of input
--   In this case, it compare the targets and guesses
correctCards :: [Card] -> [Card] -> Int
correctCards _ [] = 0
correctCards target guess = length [x |x <- guess, y <- target, x == y]


-- | A generic match function to match card with same suit or rank.
--   each suit/rank could be only used for matching once as mentioned in 
--   the project spec.
oneMatch :: Eq a => [a] -> [a] -> Int
oneMatch [] _ = 0
oneMatch target guess
    | elem next guess  = 1 + oneMatch (drop 1 target) (delete next guess)
    | otherwise        = oneMatch (drop 1 target) guess
    where next = head target


-- | * Int, represent which process it go through can only be a number in [0..4]
--   * Int, previous number of exact
--   * [Int], represent number of card for each suit, it contains 4 Int, they are representing Club Diamond Heart Spade from left to right
--   * [Card], this represent the card before the the one that we provided with feedback
--   * [rank] the upper and lower bound of rank
--   * [Card] card must in guess
--   * [Card] card must not in guess
--   * [[Card]], all candidates
type GameState = (Int, Int, [Int], [Card], [Rank], [Card], [Card], [[Card]])


-- |takes the number of cards in the answer as input and returns 
--  a pair of an initial guess, which should be a list of the specified number of cards, 
--  and a game state. The number of cards specified will be 2 
--  for most of the test, and 3 or 4 for the remaining tests
initialGuess :: Int -> ([Card],GameState)
initialGuess n = ((getCards n []), (1,0,[],[],[],[],[], []))

-- |take n adjacent cards for initial guessing
getCards :: Int -> [Card] -> [Card]
getCards 0 cardList = cardList
getCards n cardList
    | length cardList == 0  = getCards (n-1) [Card Club R3]
    | otherwise             = getCards (n-1) $ [succ $ head cardList] ++ cardList


-- | takes as input a pair of the previous guess and game state, and the feedback 
-- to this guess as a quintuple of counts of correct cards, low ranks, correct 
-- ranks, high ranks, and correct suits, and returns a pair of the next guess
-- and new game state. The means of the variables refers to the comment for GameState.
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (previous, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) (exact,lower,sameR,higher,sameS)
    -- if there is a set of useless guess and we haven't handled it yet, put them in card we should not consider.
    | (exact == 0) && (notElem (previous !! 0) cmustNot) && (length candidates == 0)
        = nextGuess (previous, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot++previous, candidates)) feedback
    -- if new guess does not make any improvement, we should throw those new changes away.
    | (lastExact == exact) && ((length previous) - exact == length suspicious) && (notElem (suspicious !! 0) cmustNot)
        =  nextGuess (previous, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot++suspicious, candidates)) feedback
    --  when we found a card we should add it to cmust
    | (process == 4) && (exact /= lastExact) && ((abs $ lastExact - exact) == (length $ previous \\ beforePre))
        = guessCards (previous, (5, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) feedback
    -- guess suits and rank range
    | process < 3
        = guessRank (previous, (process, exact, snum, previous, rbounds, cmust, cmustNot, candidates)) feedback
    -- guessing answer using the card from candidates
    | process > 3
        = (head candidates, (4, exact, snum, previous, rbounds, cmust, cmustNot, drop 1 candidates))
    -- generate new candidates when finishing rank range detection and suit pattern detection
    | otherwise
        = nextGuess (previous, (4, exact, snum, previous, rbounds, cmust, cmustNot, newCandidates)) feedback
    where
          newCandidates = updateCandidates (previous, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates))
          suspicious = previous \\ cmust
          feedback = (exact,lower,sameR,higher,sameS)


-- | this function can gradually explore the range of the answers
guessRank :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessRank (previous, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) (exact,lower,sameR,higher,sameS)
    -- we have some cards in our guess out side of the range at both upper and lower boundary
    | l && h && (length snum >= 3)
        = guessEachSuit (expand, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) feedback
    -- we have cards sit outside of the lower boundary
    | l && (length snum >= 3)
        = guessEachSuit (lowerMin, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) feedback
    --  we have cards sit outside of the upper boundary
    | h && (length snum >= 3)
        = guessEachSuit (higherMax, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) feedback
    | length snum < 3
        = guessEachSuit (previous, (process, lastExact, snum, beforePre, rankRange, cmust, cmustNot, candidates)) feedback
    --  we finish range detection
    | otherwise
        = guessEachSuit (previous, (2, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) feedback
    
    where l = lower /= 0
          h = higher /= 0
          notAll = sameR < 1
          feedback = (exact,lower,sameR,higher,sameS)
          rankRange = [rank $ minRank previous, rank $ maxRank previous]

          shrink = improveRank (improveRank avo minRank succ) maxRank pred
          avo = colliAvo previous 0 allSuits
          highn = maxnRank avo higher
          lown = minnRank avo lower
          pn = \x -> map pred x
          sn = \x -> map succ x

          expand = improveRank (improveRank avo minRank pred) maxRank succ
          lowerMin = (pn $ lown) ++ (avo \\ lown)
          higherMax = (sn $ highn) ++ (avo \\ highn)


-- | avoid card collision when guessing
colliAvo :: [Card] -> Int -> [Suit] -> [Card]
colliAvo [] _ _ = []
colliAvo [x] n slist = [changeSuit (slist !! n) x]
colliAvo (x:xs) n slist = (colliAvo [x] n slist) ++ (colliAvo xs (mod (n+1) 4) slist)


-- | guess number card in answer for each suit
guessEachSuit :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessEachSuit (nextToGuess, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) (exact,lower,sameR,higher,sameS)
    -- we found all the patterns
    | (length snum == 3)
        = guessEachSuit (nextToGuess, (process, lastExact, snum ++ [sameS], beforePre, rbounds, cmust, cmustNot, candidates)) (exact,lower,sameR,higher,sameS)
    | (length snum < 3)
        = (shiftedCard, (process, lastExact, snum ++ [sameS], beforePre, rbounds, cmust, cmustNot, candidates))
    --  we finish all the early detection, can start to guess the card now
    | (process == 2) && (length snum == 4)
        = (nextToGuess, (3, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates))
    | otherwise                     
        = (nextToGuess, (0, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates))
    where shiftedCard = map (\x -> changeSuit (succ $ suit x) x) nextToGuess


-- | this function will make use of the feedback and gradually shrink the size of candidates
guessCards :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
guessCards (previous, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) (exact,lower,sameR,higher,sameS)
    | exact - lastExact > 0 = (head newCan1, (process, exact, snum, beforePre, rbounds, cmust++mustHave1, cmustNot, drop 1 newCan1))
    | otherwise             = (head newCan2, (process, exact, snum, beforePre, rbounds, cmust++mustHave2, cmustNot, drop 1 newCan2))
        where
              newCan1    = updateCandidates (previous, (process, exact, snum, beforePre, rbounds, cmust++mustHave1, cmustNot, candidates))
              mustHave1  = previous \\ beforePre
              newCan2    = updateCandidates (previous, (process, exact, snum, beforePre, rbounds, cmust++mustHave2, cmustNot, candidates))
              mustHave2  = beforePre \\ previous


--  we use the information so far to generate new set of candidates, the reason that I did't use filter is because it really costly 
--  to go through the whole list.
updateCandidates :: ([Card],GameState) -> [[Card]]
updateCandidates (previous, (process, lastExact, snum, beforePre, rbounds, cmust, cmustNot, candidates)) = cd
    where
        -- | variables
        oldCeiling     = maximum rbounds
        oldFloor       = minimum rbounds
        newCeiling     = rank $ maxRank previous
        newFloor       = rank $ minRank previous
        upperChange    = oldCeiling < newCeiling
        lowerChange    = oldFloor > newFloor

        -- | generate candidates
        -- card we need to consider in our candidates, e.g., if we know 1 card in the answer, support we need to guess 4 cards, now
        -- we only need to consider the other 3.
        cneed = length previous - length cmust
        -- throw those cards that are out of range
        reducedDeck = filter (\x -> notElem x cmust) $ filter (\x -> rank x >= newFloor) $ filter (\x -> (rank x) <= newCeiling) (allCards \\ cmustNot)
        extraFunction = boundChecker [newFloor, newCeiling] upperChange lowerChange
        cd = getCandidates cneed cmust reducedDeck snum rbounds extraFunction


-- check whether the upper bound or lower bound has changed after fining the rank range of the answers
boundChecker :: [Rank] -> Bool -> Bool -> ([Card] -> Bool)
boundChecker rbounds upperChange lowerChange
    | upperChange && lowerChange
        = \x -> elem l (map rank x) && elem h (map rank x)
    | upperChange
        = \x -> elem h (map rank x)
    | lowerChange
        = \x -> elem l (map rank x)
    | otherwise
        = \x -> True
    where
        l = minimum rbounds
        h = maximum rbounds


--  use given information to generate candidates
-- * cneed, number of card we need for each set except the cmust
-- * cmust, card we must include in each candidate
-- * reducedDeck, possible cards
-- * snum
getCandidates :: Int -> [Card] -> [Card] -> [Int] -> [Rank] -> ([Card] -> Bool) -> [[Card]]
getCandidates cneed cmust reducedDeck snum rbounds func
    | length cmust == 0     = generateCandidatesWithCondition f cneed reducedDeck []
    | otherwise             = [x++cmust | x <- generateCandidates cneed reducedDeck [], f (x++cmust)]
    where f = \x -> (func x) && (suitMatcher x snum)


-- | generate candidates based on a subset of allCards which is seed
generateCandidates :: Int -> [Card] -> [[Card]] -> [[Card]]
generateCandidates 0 _ candidates = candidates
generateCandidates n seed [] = generateCandidates (n-1) seed [[x]| x <- seed]
generateCandidates n seed candidates = generateCandidates (n-1) seed (nub $ map sort [x++[y]| x <- candidates, y <- seed, notElem y x])

-- | generate candidates based on a subset of allCards which is seed and satisfy the condition f
generateCandidatesWithCondition :: ([Card] -> Bool) -> Int -> [Card] -> [[Card]] -> [[Card]]
generateCandidatesWithCondition _ 0 _ candidates = candidates
generateCandidatesWithCondition f n seed [] = generateCandidatesWithCondition f (n-1) seed [[x]| x <- seed]
generateCandidatesWithCondition f 1 seed candidates = generateCandidatesWithCondition f 0 seed (nub $ map sort [x++[y]| x <- candidates, y <- seed, notElem y x, f (x++[y])])
generateCandidatesWithCondition f n seed candidates = generateCandidatesWithCondition f (n-1) seed (nub $ map sort [x++[y]| x <- candidates, y <- seed, notElem y x])

-- | whether at least 1 set in the first argument are in the second argument
containsOne :: [[Card]] -> [Card] -> Bool
containsOne [] _ = True
containsOne [x] cards = allIn x cards
containsOne (x:xs) cards = allIn x cards || containsOne xs cards


-- | test whether all elements in first argument are in second argument
allIn :: [Card] -> [Card] -> Bool
allIn [] b = True
allIn [x] b = elem x b
allIn (x:xs) b = (elem x b) && (allIn xs b)


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

-- | top n cards with top n highest rank
maxnRank :: [Card] -> Int -> [Card]
maxnRank cards 0 = []
maxnRank cards n = [mr] ++ (maxnRank remains (n-1))
    where mr = maxRank cards
          remains = delete mr cards


-- | top n cards with top n lowest rank
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


-- | this represent all the cards defined in Card.hs
allCards :: [Card]
allCards = enumFromTo (toEnum 0::Card) (toEnum 51::Card)

-- | return all the suit which defined in Card.hs
allSuits :: [Suit]
allSuits = [suit $ Card Club R2, suit $ Card Heart R2,suit $ Card Diamond R2,suit $ Card Spade R2]

-- | change the suit of the card to given suit
changeSuit :: Suit -> Card -> Card
changeSuit news (Card s r) = Card news r


-- | the function will examine whether the given candidates can match the patter of the given suit
--   pattern which are represented in the same form as in GameState
--   Club Diamond Heart Spade from head to tail
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

