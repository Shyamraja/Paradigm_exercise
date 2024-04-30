module Main where

import TwentyOne

main :: IO ()
main = do
    putStrLn "Welcome to 21 Card Game!"
    -- Create and shuffle a new deck
    deck <- newShuffledDeck  
     -- Initialize the game with the shuffled deck
    let initialState = initGame deck 
    -- Begin the game loop
    playGame initialState  

playGame :: GameState -> IO ()
playGame gameState = do
    putStrLn $ "Current game state:\n" ++ drawGame gameState
    if gameOver gameState
    then endGame gameState
    else do
        putStrLn "Enter 'h' to hit or 's' to stand:"
        input <- getLine
        let newState = handleInput input gameState
        -- Continue the game loop with the new state
        playGame newState  
endGame :: GameState -> IO ()
endGame gameState = do
    putStrLn "Game over!"
    putStrLn $ "Final game state:\n" ++ drawGame gameState
    -- Determine and announce the winner
    putStrLn $ "Winner: " ++ determineWinner gameState  

determineWinner :: GameState -> String
determineWinner gameState
    | playerBusted gameState = "Dealer wins"
    | dealerBusted gameState = "Player wins"
    | playerScore > dealerScore = "Player wins"
    | playerScore < dealerScore = "Dealer wins"
    | otherwise = "It's a tie"
    where
        playerScore = sumCards (playerHand gameState)
        dealerScore = sumCards (dealerHand gameState)
