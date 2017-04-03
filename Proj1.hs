--Declarative Project 1 2016 Semester 2
--Author: Kaven Peng
--Student ID: 696573
--Version 1.0

module Proj1 (feedback, initialGuess, nextGuess, GameState)
where

--imports
import Card
import Data.List
import qualified Data.Set as Set

--GameState type declaration
data GameState = GameState {possible::[(Set.Set Card)]}
                deriving (Eq,Show)

--Gives feedback in the form of a 5-tuple
--given answer and guess lists must be same length and non-empty
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [] [] = error "Empty Guess and Answer"
feedback a g
  | (length a) == (length g) 
    = (correctCard a g, lowerRank a g, correctRank a g, higherRank a g, correctSuit a g)
  | otherwise 
    = error "Answer and Guess different length"

--Finds the number of correct cards in guess after comparing to the answer
--done by finding the size of the intersection of answer and guess sets
correctCard :: [Card] -> [Card] -> Int
correctCard a g 
  = Set.size $ Set.intersection (Set.fromList a) (Set.fromList g)

--Finds the number of cards in answer lower in rank than lowest rank in guess
--done by finding a list of all cards in answer that have 
--lower rank than lowest rank in guess and then finding the length of that list
lowerRank :: [Card] -> [Card] -> Int
lowerRank a g 
  = length [ c | c <- a, (rank c) < (minimum $ map rank g)]

--Finds number of cards of equal rank in guess and answer
--by checking if the rank is in the answer then removing the first equally ranked card
--from answer and recursing over the new answer list
correctRank :: [Card] -> [Card] -> Int
correctRank [] [] = 0
correctRank _ [] = 0
correctRank (a:as) (g:gs)
  | (rank g) `elem` (map rank (a:as)) 
    = 1 + correctRank (deleteBy equalRank g (a:as)) gs
  | otherwise = correctRank (a:as) gs
  where 
    equalRank x y = rank x == rank y

--Finds number of cards in answer higher in rank than highest rank in guess
--done by finding a list of all cards in the answer that have
--higher rank than highest rank in guess then finding the length of the list
higherRank :: [Card] -> [Card] -> Int
higherRank a g
  = length [ c | c <- a, (rank c) > (maximum $ map rank g)]

--Finds number of cards of equal suit in guess and answer
--by checking if the suit is in the answer then removing the first card with equal suit
--in answer and recursing over the new answer list 
correctSuit :: [Card] -> [Card] -> Int
correctSuit [] [] = 0
correctSuit _ [] = 0
correctSuit (a:as) (g:gs)
  | (suit g) `elem` (map suit (a:as))
    = 1 + correctSuit (deleteBy equalSuit g (a:as)) gs
  | otherwise = correctSuit (a:as) gs
  where 
    equalSuit x y = suit x == suit y

--Gets all possible card combinations by hardcoding possible n-combinations
--namely 2-4 cards by creating a list of all permutations using list comprehension
--then removing repeats by converting to sets and back again, also filtering for size n
--sets to get rid of possible repeated cards in choices
cardCombs :: Int -> [(Set.Set Card)]
cardCombs n
  | n == 2 
    = filter ((n==).(Set.size)) (Set.toList $ Set.fromList 
    [ Set.fromList [c1,c2] | c1<-deck, c2<-deck ])
  | n == 3 
    = filter ((n==).(Set.size)) (Set.toList $ Set.fromList 
    [ Set.fromList [c1,c2,c3] | c1<-deck, c2<-deck, c3<-deck])
  | n == 4
    = filter ((n==).(Set.size)) (Set.toList $ Set.fromList 
    [ Set.fromList [c1,c2,c3,c4] | c1<-deck, c2<-deck, c3<-deck, c4<-deck])
  where 
    deck = [minBound .. maxBound] :: [Card]

--hardcoded initial guesses and GameState
initialGuess :: Int -> ([Card],GameState)
initialGuess n
  | n == 2 
    = (read "[6C,TH]", GameState (cardCombs n))
  | n == 3 
    = (read "[4C,7S,TH]", GameState (cardCombs n))
  | n == 4 
    = (read "[4C,6S,8H,TD]",GameState (cardCombs n))

--Filters the possible combinations in GameState 
--given a previous guesses correctCard value and guess/GameState
--returns a new filtered GameState representing possible combinations left
filterCC:: Int -> ([Card],GameState) -> ([Card],GameState)
filterCC n (c, state)
  | n == 0 
    = (c, GameState (filter (not.hasIntersects) (possible state)))
    --get all possible combinations with no intersections with current guess
  | n == length c
    = (c, GameState [Set.fromList c])
    --guess is correct
  | otherwise
    = (c, GameState (delete (Set.fromList c) (filter hasIntersects (possible state))))
    --has some correct cards, so filter out all cards that don't have any of the current
    --cards in the guess including the current guess using intersections
  where 
    hasIntersects s = not $ (Set.null) $ (Set.intersection s (Set.fromList c))
    --simple predicate test that returns true if the intersection of a given set
    --and current guess is non-empty, in other words sets and guess have an intersection'

--Filters possible combinations in GameState
--given correctRank value
filterCR :: Int -> ([Card],GameState) -> ([Card],GameState)
filterCR n (c, state)
  | n == 0
    = (c, GameState (filter noRank (possible state)))
  --get all combinations with no intersections with current guess
  | n == length c
    = (c, GameState (filter equalRanks (possible state)))
  --get all combinations where all ranks are equal to guess 
  | otherwise
    = (c, GameState (filter 
      (\s -> not (noRank s || equalRanks s)) (possible state)))
  --get all combinations where there are intersections with current guess and
  --no combinations with the same exact ranks as guess
  where 
    cRank = Set.map rank (Set.fromList c)
    --set of ranks of guess
    noRank s = (Set.null) $ (Set.intersection (Set.map rank s) cRank)
    --True if no ranks are shared between the guess and a combination
    equalRanks s = (sort (map rank $ Set.toList s)) == (sort (map rank c))
    --True if combination has the exact same ranks as the guess

--Filters possible combinations in GameState
--given correctSuit value
filterCS :: Int -> ([Card],GameState) ->([Card],GameState)
filterCS n (c, state)
  | n == 0
    = (c, GameState (filter noSuit (possible state)))
    --all combinations with none of the guess suits
  | n == length c
    = (c, GameState (filter equalSuits (possible state)))
    --all combinations with same suits
  | otherwise
    = (c, GameState (filter 
      (\s -> not (noSuit s || equalSuits s)) (possible state)))
    --all combinations which contains any of the guess suits and
    --is different from the current guess
  where
    cSuit = Set.map suit (Set.fromList c)
    --set of suits in guess
    noSuit s = (Set.null) $ (Set.intersection (Set.map suit s) cSuit)
    --True if s does not contain any of the suits in guess
    equalSuits s = (sort (map suit $ Set.toList s)) == (sort (map suit c))
    --True if s has exactly the same suits as guess

--Filters possible combinations in GameState
--given a tuple of lowerRank and higherRank values
filterLH :: (Int,Int) -> ([Card],GameState) -> ([Card],GameState)
filterLH (lo, hi) (c, state)
  | (lo,hi) == (0,0) 
    = (c, GameState (filter 
      (\s -> betweenRank s cMin cMax) (possible state)))
    --when both lo hi are 0 this means the all ranks of answer are within
    --bounds cMin and cMax which are the min and max rank of the guess
  | lo == length c
    = (c, GameState (filter 
      (\s -> betweenRank s minBound (pred cMin)) (possible state)))
    --when lo value is exactly size of the guess then this means that
    --all answer ranks are less than cMin non-inclusive
  | hi == length c
    = (c, GameState (filter 
      (\s -> betweenRank s (succ cMax) maxBound) (possible state)))
    --when hi value is exactly size of the guess then this means that
    --all answer ranks are greater than cMax non-inclusive
  | lo + hi == length c
    = (c, GameState (filter 
      (\s -> outsideRank s (pred cMin) (succ cMax)) (possible state)))
    --when lo + hi is equal to size of guess this means that
    --no answer ranks are within the bounds of [cMin..cMax] so we find
    --all ranks outside those bounds.
  | (lo > 0) && (hi == 0)
    = (c, GameState (filter 
      (\s -> betweenRank s minBound cMax) (possible state)))
    --in this case answer ranks are between minBound and cMax, so at best
    --we can only filter out any results greater than cMax.
  | (lo == 0) && (hi > 0)
    = (c, GameState (filter 
      (\s -> betweenRank s cMin maxBound) (possible state)))
    --this case is opposite of the case directly above,
    --we can only filter out any results less than cMin
  | otherwise
    --this case is when there are both values less than cMin and greater than cMax
    --while also containing values between cMin and cMax
    --when this is the case we cannot filter out anything
    = (c, state)
  where
    cMin = minimum $ map rank c
    --min of guess
    cMax = maximum $ map rank c
    --max of guess
    betweenRank s min max 
      = all (\x -> x `elem` [min..max]) (Set.toList (Set.map rank s))
    --predicate to check if all of set s ranks are within min-max bounds
    outsideRank s x y 
      = all (\z -> z `elem` [minBound..x]++[y..maxBound]) (Set.toList (Set.map rank s))
    --predicate to check if all of set s ranks are outside of x-y bounds non-inclusive

--Picks the next guess given a guess/GameState and it's feedback
--done by first running the GameState and feedback through all filters
--then choosing the next guess from the updated GameState
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (c, state) (cc,lo,cr,hi,cs)
  = (chooseGuess nextState, nextState)
    where
      (_,nextState) = filterLH (lo,hi) $ filterCR cr $ 
                      filterCS cs $ filterCC cc (c, state)

--Chooses next guess from GameState, here we simply take the middle element
--and convert it back into a list, note: this won't work on empty list
--however I have designed the filtering algorithms to never filter to empty
--so this shouldn't happen
chooseGuess :: GameState -> [Card]
chooseGuess gs
    = Set.toList $ head $ snd $ splitAt ((length (possible gs)) `div` 2) (possible gs)
