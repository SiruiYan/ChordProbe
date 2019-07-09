## Description
This Haskell program simulates the game of ChordProbe.

For a ChordProbe game, one player will be the composer and the other is the performer. The composer begins by selecting a three-pitch musical chord, where each pitch comprises a musical note, one of A, B, C, D, E, F, or G, and an octave, one of 1, 2, or 3. This chord will be the target for the game. The order of pitches in the target is irrelevant, and no pitch may appear more than once. This game does not include sharps or flats, and no more or less than three notes may be included in the target.

Once the composer has selected the target chord, the performer repeatedly chooses a similarly defined chord as a guess and tells it to the composer, who responds by giving the performer the following feedback:
1. how many pitches in the guess are included in the target (correct pitches)
2. how many pitches have the right note but the wrong octave (correct notes)
3. how many pitches have the right octave but the wrong note (correct octaves)

In counting correct notes and octaves, multiple occurrences in the guess are only counted as correct if they also appear repeatedly in the target. Correct pitches are not also counted as correct notes and octaves. For example, with a target of A1, B2, A3, a guess of A1, A2, B1 would be counted as 1 correct pitch (A1), two correct notes (A2, B1) and one correct octave (A2). B1 would not be counted as a correct octave, even though it has the same octave as the target A1, because the target A1 was already used to count the guess A1 as a correct pitch.

## How to run
Test driver program: Proj1Test.hs
Test using the command: ghc -O2 --make Proj1Test
To run Proj1Test, give it the target as three separate command line arguments, for example ./Proj1Test D1 B1 G2 would search for the target ["D1", "B1", "G2"]. It will then use your Proj1 module to guess the target; the output will look something like:
Your guess 1:  ["A1","B1","C2"]
My answer:  (1,0,2)
Your guess 2:  ["A1","D1","E2"]
My answer:  (1,0,2)
Your guess 3:  ["A1","F1","G2"]
My answer:  (1,0,2)
Your guess 4:  ["B1","D1","G2"]
My answer:  (3,0,0)
You got it in 4 guesses!
