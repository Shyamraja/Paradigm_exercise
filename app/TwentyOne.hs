module TwentyOne (
    Rank(..),
    Suit(..),
    Card,
    Deck,
    GameState(..),
    newDeck,
    newShuffledDeck,
    initGame,
    playerHit,
    dealerHit,
    playerStand,
    drawGame,
    handleInput,
    update,
    sumCards  
) where

import System.Random.Shuffle (shuffleM)

-- Define all ranks and suits
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Show, Eq)
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
type Card = (Suit, Rank)
type Deck = [Card]

-- Define the game state
data GameState = GameState {
    deck :: Deck,
    playerHand :: [Card],
    dealerHand :: [Card],
    gameOver :: Bool,
    playerBusted :: Bool,
    dealerBusted :: Bool
} deriving (Show)

-- Create a new deck of cards
newDeck :: Deck
newDeck = [(suit, rank) | suit <- [Hearts, Diamonds, Clubs, Spades], rank <- [Two .. Ace]]

-- Shuffling a new deck of cards
newShuffledDeck :: IO Deck
newShuffledDeck = shuffleM newDeck

-- Deal a specified number of cards from the deck
dealCards :: Int -> Deck -> ([Card], Deck)
dealCards n deck = splitAt n deck

-- Initialize the game state with initial card deals
initGame :: Deck -> GameState
initGame deck = let (playerCards, deck1) = dealCards 2 deck
                    (dealerCards, deck2) = dealCards 2 deck1
                in GameState deck2 playerCards dealerCards False False False

--Function for  Player hits to take  card from the deck
playerHit :: GameState -> GameState
playerHit game
    | gameOver game = game
    | null (deck game) = game -- Handle empty deck scenario
    | otherwise = let (card:newDeck) = deck game
                      newHand = card : playerHand game
                      busted = sumCards newHand > 21
                  in game { deck = newDeck, playerHand = newHand, playerBusted = busted, gameOver = busted }

-- Here Dealer can also hits to take a card from the deck if player stands
dealerHit :: GameState -> GameState
dealerHit game
    | gameOver game || sumCards (dealerHand game) >= 17 = game
    | null (deck game) = game -- Handle empty deck scenario
    | otherwise = let (card:newDeck) = deck game
                      newHand = card : dealerHand game
                      busted = sumCards newHand > 21
                  in dealerHit game { deck = newDeck, dealerHand = newHand, dealerBusted = busted }

-- Player stands, transitioning to the dealer's turn to take card from deck
playerStand :: GameState -> GameState
playerStand game = dealerTurn game

-- Rendering  the game state as a string for display
drawGame :: GameState -> String
drawGame gameState = "Player Hand: " ++ show (playerHand gameState) ++ "\nDealer Hand: " ++ show (dealerHand gameState)

-- Handling the keyboard inputs for player decisions
handleInput :: String -> GameState -> GameState
handleInput "h" game = playerHit game
handleInput "s" game = playerStand game
handleInput _ game = game

-- Processing the  dealer's turn logic after again when dealer stands
dealerTurn :: GameState -> GameState
dealerTurn game
    | not (gameOver game) && sumCards (dealerHand game) < 17 = dealerHit game
    | otherwise = game { gameOver = True }

-- Updating the function, stub for potential future use
update :: Float -> GameState -> GameState
update _ = id

-- Helper function to calculate the sum of card ranks
sumCards :: [Card] -> Int
sumCards cards = sum [value rank | (_, rank) <- cards]
    where
        value Ace = 11
        value rank
            | rank `elem` [Jack, Queen, King] = 10
            | otherwise = fromEnum rank + 2
