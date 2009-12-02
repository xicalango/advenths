module Main where

--import qualified Data.Map as M
--import qualified Data.Set as S

import Control.Monad.State
import System.IO

import GameState
import Action

{-item = Item { iID = "lantern"
            , iPre = "a brass"
            , iName = "lantern"
            , iDesc = "A small, brass lantern."
            , iEvents = M.fromList [(EvLookAt, S.fromList [(IF (InRoom "bla") (Message "hello"))]) ]
      }-}

playGame :: Game ()
playGame = do
  showCurrentRoom
  lift $ putStr "> "
  line <- lift $ getLine
  doAction $ parseInput $ words line
  where
    showCurrentRoom :: Game ()
    showCurrentRoom = do
      state <- get
      room <- getRoom $ gsInRoom state
      lift $ printRoom room
      printItemList "There is" $ rItems room
      if rVisited room
        then return()
        else updateRoom room{ rVisited=True }

doGame :: Game ()
doGame = do
  setupGame
  forever $ playGame
  where
    setupGame = do
    roomfile <- lift $ readFile "rooms.dat"
    itemfile <- lift $ readFile "items.dat"
    buildGame (read roomfile) (read itemfile)
    setRoom "main"

main :: IO ()
main = do
  --putStrLn $ show item
  hSetBuffering stdout NoBuffering
  evalStateT (doGame) $ emptyGameState

