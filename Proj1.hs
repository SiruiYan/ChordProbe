-- File        : Proj1.hs
-- Author      : Sirui Yan 
-- Student ID  : 792320
-- Purpose     : Implementation of initialGuess and nextGuess 
--               and playing ChordProbe types

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List


-- Implementaion of playing ChordProbe types, including types 
-- for note, octave and pitch.

-- A musical note, ordered alphabetically.
data Note = A | B | C | D | E | F | G
 deriving (Eq, Ord, Show, Enum, Bounded)

-- An octave, ordered by number by ascending order. 
data Octave = R1 | R2 | R3
 deriving (Eq, Ord, Enum, Bounded)

-- A musical pitch, comprised of a musical note and an octave.
data Pitch = Pitch {note::Note, octave::Octave}
 deriving (Eq, Bounded, Ord)

-- Derived show instance for type octave
instance Show Octave where
 show o = [octavechars !! fromEnum o]
  where octavechars = "123"

-- Derived show instance for type pitch
instance Show Pitch where
 show (Pitch n o) = show n ++ show o

-- Derived enum instance for type pitch
-- Pitches are enumerated with the the order of a pitch's 
-- note first and then the order of octave.
instance Enum Pitch where
 fromEnum (Pitch n o) = fromEnum n * 3 + fromEnum o
 toEnum n = Pitch notee octavee
  where
   notee = toEnum (n `div` 3)
   octavee = toEnum (n `mod` 3)

-- A chord is a list of pitches comprised of three pitches each
type Chord = [Pitch]
type GameState = [Chord]



-- Implementation of initialGuess

-- InitialGuess function that takes no input arguments and returns a pair of 
-- hardcoded first guess and initial GameState.
initialGuess :: ([String],GameState)
initialGuess = (["A1","B1","C2"],initialState)

-- InitialState function that returns the initial gamestate, which includes
-- all the possible chords.
initialState :: GameState
initialState = [[x,y,z] | x <- candidate, y <- candidate, z <- candidate, x < y, y < z]
 where
  candidate = [minBound::Pitch .. maxBound::Pitch]


-- Implementation of nextGuess

-- nextGuess function that takes a tuple of previous guess and game state,
-- and the feedback to this guess as a triple of correct pitches, notes, 
-- and octaves, and returns a tuple of the next guess and game state.
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (guess,gs) result = (ng,ns)
 where
  ns = filter (\x -> feedback (readGuess guess) x == result) gs
  ng = showGuess (findBestGuess ns)

-- FindBestGuess function that takes a game state as input, and returns the best
-- guess of chord in this game state. The best guess in this function is the chord
-- that is expected to leave the least number of remianing chords if it is target.
findBestGuess :: GameState -> Chord
findBestGuess [] = error "No chord in game state"
findBestGuess gs
 | index < 0 = error "No chord in game state"
 | otherwise = gs !! index
 where 
  expectation = map (expectedRemain gs) gs
  index = readListIndex (minimum expectation) expectation

-- Feedback function that takes the target chord and guess chord, and returns
-- the feedback as a triple of correct pitches, notes, and octaves.
feedback :: Chord -> Chord -> (Int,Int,Int)
feedback t g = (rightPitch, rightNote, rightOctave)
 where
  rightPitch = length (t `intersect` g)
  rightNote = length g - length (deleteFirstsBy eqNote t g) - rightPitch
  rightOctave = length g - length (deleteFirstsBy eqOctave t g) - rightPitch

-- ExpectedRemain takes a guess and a game state as input, and returns expected 
-- reamining number of chord in game state. To calculate the expected remaining number
-- of chord, we need to konw the count of every possible feedback got from the guess and
-- the game state, which is countResult. It equals to the sum of square of the count of 
-- every possible feedback, and then is divided by the number of chord in game state. 
-- Since all the expected remianing are divided by the same number, it makes no difference
-- to the result of comparision. Thus, we only compare the sum.
expectedRemain ::  GameState -> Chord -> Int
expectedRemain [] _ = error "No chord in game state"
expectedRemain g t = sum (zipWith (*) countResult countResult)
 where
  range = [0,1,2,3]
  possibleAnswer = [ (x,y,z) | x<-range, y<-range, z<-range, x+y<=3, x+z<=3]
  answerCount = replicate (length possibleAnswer) 0
  countResult = foldl (count t possibleAnswer) answerCount g

-- Count takes a target, a guess, a list of answer count, and the list of possible answers
-- as input, and returns the new list answer count.
count :: Chord -> [(Int,Int,Int)] -> [Int] -> Chord  -> [Int]
count t pa ac g = addOnNthElement index ac
 where index = readListIndex (feedback t g) pa

-- Add 1 at the Nth element of the input list
addOnNthElement :: Int -> [Int] -> [Int]
addOnNthElement n a = a1 ++ ((num+1):a2)
 where (a1,num:a2) = splitAt n a

-- Returns if two pitches have the same note
eqNote :: Pitch -> Pitch -> Bool
eqNote p1 p2 = note p1 == note p2

-- Returns if two pitches have the same octave
eqOctave :: Pitch -> Pitch -> Bool
eqOctave p1 p2 = octave p1 == octave p2

-- Show the guess in String
showGuess :: Chord -> [String]
showGuess = map show

-- Read a guess from String
readGuess :: [String] -> Chord
readGuess = map readPitch

-- Read a pitch from String
readPitch :: String -> Pitch
readPitch c
 | length c /= 2 = error "Invalid pitch length"
 | otherwise = toEnum index
 where
  notechars = "ABCDEFG"
  octavechars = "123"
  index = readListIndex (head c) notechars * 3 + readListIndex (c!!1) octavechars

-- Read the index of a element in a list
readListIndex :: Eq a => a -> [a] -> Int
readListIndex e l =
 case elemIndex e l of
  Nothing -> error "Invalid element in list"
  Just i -> i
