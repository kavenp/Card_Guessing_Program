# Card_Guessing_Program
Built for Declarative Programming Project 1

A program that tries to a combination of cards given as arguments as fast as possible.

Build by running
ghc -O2 --make Proj1test

Then running
./Proj1test 2H 3C JS AD

Here the card combination given is 2 of Hearts, 3 of Clubs, Jack of Spades and Ace of Diamonds.
Below are all the number values of the cards:

  23456789TJQKA

It is recommended to stay below 4 cards as permutations are in the order of factorial growth.
However it can still guess the answer relatively quickly for 3-4 card combinations.
