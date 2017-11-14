{-

Karan Shrestha

10/08/17


CS330 - Lab 7
Fall 2017


-}


module Main where
import System.Random
import Control.Monad

-- Data type of a playing card, defining how card can be constructed
data PlayingCard = Card Rank Suit
                        deriving (Eq)
type Rank = Integer
data Suit = Clubs | Diamonds | Hearts | Spades
                        deriving (Eq, Ord, Enum, Show)

-- Show card instance, where the integer value of the rank of the card is properly shown as Aces, Jacks, Queens, or Kings if it is larger than 10.
-- This is for printing out the result after every round
instance Show PlayingCard where
 show (Card rank suit) = cardRank rank ++ " " ++ cardSuit suit where
  cardRank r | r > 1 && r < 11 = show r
             | r == 11 = "Jack"
             | r == 12 = "Queen"
             | r == 13 = "King"
             | r == 14 = "Ace"
  cardSuit s = show s

-- Type of functions that evaluate the two players' cards before every round to determine if a winner exists
type CheckForWinner = (String -> ([PlayingCard], [PlayingCard]) -> IO ()) -> ([PlayingCard], [PlayingCard]) -> IO ()

-- Shuffler
shuffleDeck :: [PlayingCard] -> IO [PlayingCard]
shuffleDeck deck =
 if (length deck < 2)
  then return deck
 else do
  i <- randomRIO (0, length(deck)-1)
  r <- shuffleDeck (take i deck ++ drop (i+1) deck)
  return (deck!!i : r)

-- Deal equal number of cards at the beginning to the two players
dealTwoStacks :: IO ([PlayingCard], [PlayingCard])
dealTwoStacks = do
             let originalDeck = [Card rank suit | rank <- [2 .. 14], suit <- [Clubs .. Spades]]
             deck <- shuffleDeck originalDeck
             return (splitAt 26 deck)

-- Compare two cards by comparing their rank values
compareCard :: PlayingCard -> PlayingCard -> Bool
compareCard (Card val1 _) (Card val2 _)
 | val1 > val2 = True
 | otherwise   = False

-- Depending on the game mode (auto or interactive), a different "check for winner" function is called
gameMode :: String -> CheckForWinner
gameMode mode =
 if (mode == "auto")
  then autoGameEnd
 else interactiveGameEnd

-- With autoWar, we do not need to ask the user after every turn whether or not they want to continue
-- The program will just run until one or both of the players run out of cards (game will be over), which is checked after every round
autoGameEnd :: CheckForWinner
autoGameEnd round (p1, p2) = do
     case () of _
                  | (p1, p2) == ([], []) -> putStrLn "Game Over. Draw."
                  | (p1, p2) == (p1, []) -> putStrLn "Game Over. Player 1 Wins!"
                  | (p1, p2) == ([], p2) -> putStrLn "Game Over. Player 2 Wins!"
                  | otherwise            -> round "auto" (p1, p2)

-- In addition to checking for a winner, interactiveWar also needs to ask the user after every round if they wish to continue the game
interactiveGameEnd :: CheckForWinner
interactiveGameEnd round (p1, p2) = do
  let checkGameEnd = do
             case () of _
                         | (p1, p2) == ([], []) -> putStrLn "Game Over. Draw."
                         | (p1, p2) == (p1, []) -> putStrLn "Game Over. Player 1 Wins!"
                         | (p1, p2) == ([], p2) -> putStrLn "Game Over. Player 2 Wins!"
                         | otherwise            -> round "interactive" (p1, p2)
  putStrLn "Continue to Next Round? (y/n)"
  continue <- getLine
  if (continue == "y")
   then checkGameEnd
  else
   putStrLn "Game Cancelled by Player."

-- Recursively play rounds of the game, taking in the current cards each player holds and outputing the winner of each round (or call the warRound function for war)
-- The current cards in both players' stacks are updated (add or remove card(s)) and print to screen
-- Before every round, the end condition is checked (see the autoGameEnd and interactiveGameEnd), and the player is asked if they wanted to continue if in interactive mode
-- "gameMode" is either autoGameEnd or interactiveGameEnd, specified by the first parameter of this function ("auto" or "interactive")
gameRound :: String -> ([PlayingCard], [PlayingCard]) -> IO ()
gameRound mode (p1, p2) =
 if (compareCard (head p1) (head p2))
  then do
   putStrLn ("Player 1's cards: " ++ (show p1) ++ "\n" ++ "Player 2's cards: " ++ (show p2))
   putStrLn ((show (head p1)) ++ " vs. " ++ (show (head p2)) ++ "\n" ++ "Player 1 Wins Round!" ++ "\n")
   gameMode mode gameRound ((tail p1) ++ [head p1] ++ [head p2], (tail p2))
 else
  if (compareCard (head p2) (head p1))
   then do
    putStrLn ("Player 1's cards: " ++ (show p1) ++ "\n" ++ "Player 2's cards: " ++ (show p2))
    putStrLn ((show (head p1)) ++ " vs. " ++ (show (head p2)) ++ "\n" ++ "Player 2 Wins Round!" ++ "\n")
    gameMode mode gameRound ((tail p1), (tail p2) ++ [head p1] ++ [head p2])
   else do
    putStrLn ("Player 1's cards: " ++ (show p1) ++ "\n" ++ "Player 2's cards: " ++ (show p2))
    putStrLn ((show (head p1)) ++ " vs. " ++ (show (head p2)) ++ "\n" ++ "War!" ++ "\n")
    gameMode mode warRound ((tail p1), (tail p2))

-- To go to war, each player take three cards from their stack and flip over the fourth, which is the card used for war
-- If a player doesn't have enough cards, they take however many they can and flip over their last card as the card for war
getWarStack :: [PlayingCard] -> [PlayingCard]
getWarStack stack
 | (length stack) >= 4 = take 4 stack
 | otherwise           = stack

-- The cards used for war is taken from the players' stacks, which is especially useful if war happen again during a war round
removeWarStack :: [PlayingCard] -> [PlayingCard] -> [PlayingCard]
removeWarStack s1 s2 = filter (not . flip elem s1) s2

-- Very similar to gameRound, recursively take war stacks and compare the last war stack card until someone wins, then a normal game round can start again
-- Else the players keep going to war, until someone wins or runs out of card to go to war with, which ends the game
warRound :: String -> ([PlayingCard], [PlayingCard]) -> IO ()
warRound mode (p1, p2) =
 if (compareCard (last w1) (last w2))
  then do
   putStrLn ("Player 1's war cards: " ++ (show w1) ++ "\n" ++ "Player 2's war cards: " ++ (show w2))
   putStrLn ((show (last w1)) ++ " vs. " ++ (show (last w2)) ++ "\n" ++ "Player 1 Wins War!" ++ "\n")
   gameMode mode gameRound ((tail p1) ++ w1 ++ w2, (removeWarStack w2 p2))
 else
  if (compareCard (last w2) (last w1))
   then do
    putStrLn ("Player 1's war cards: " ++ (show w1) ++ "\n" ++ "Player 2's war cards: " ++ (show w2))
    putStrLn ((show (last w1)) ++ " vs. " ++ (show (last w2)) ++ "\n" ++ "Player 2 Wins War!" ++ "\n")
    gameMode mode gameRound ((removeWarStack w1 p1), (tail p2) ++ w1 ++ w2)
  else do
   putStrLn ("Player 1's war cards: " ++ (show w1) ++ "\n" ++ "Player 2's war cards: " ++ (show w2))
   putStrLn ((show (last w1)) ++ " vs. " ++ (show (last w2)) ++ "\n" ++ "War! (again)" ++ "\n")
   gameMode mode warRound ((removeWarStack w1 p1), (removeWarStack w2 p2))
 where
  w1 = getWarStack p1
  w2 = getWarStack p2

autoWar :: IO ()
autoWar = do
         (p1, p2) <- dealTwoStacks
         gameRound "auto" (p1, p2)

interactiveWar :: IO ()
interactiveWar = do
                (p1, p2) <- dealTwoStacks
                gameRound "interactive" (p1, p2)

main :: IO ()
main = do
      putStrLn "Welcome to War!"
      interactiveWar
