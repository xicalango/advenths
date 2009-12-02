module Action ( Action(..)
              , parseInput 
              , doAction
              )

where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State
import Control.Exception

import GameState
              
data Action = Goto Dir
            | Quit
            | Look (Maybe String)
            | Inventory
            | Pickup (Maybe String)
            | Drop (Maybe String)
            | NAA String --Not an Action

parseInput :: [String] -> Action
parseInput ["north"]				= Goto North
parseInput ["south"]				= Goto South
parseInput ["east"]				= Goto East
parseInput ["west"]				= Goto West
parseInput ["n"]				= Goto North
parseInput ["s"]				= Goto South
parseInput ["e"]				= Goto East
parseInput ["w"]				= Goto West

parseInput ["look"]				= Look Nothing
parseInput ("look":"at":os)			= Look $ Just $ unwords os
parseInput ("look":os)				= Look $ Just $ unwords os

parseInput ["invent"]				= Inventory
parseInput ["pickup"]				= Pickup Nothing
parseInput ("pickup":os)			= Pickup $ Just $ unwords os
parseInput ["get"]				= Pickup Nothing
parseInput ("get":os)				= Pickup $ Just $ unwords os
parseInput ["drop"]				= Drop Nothing
parseInput ("drop":os)				= Drop $ Just $ unwords os

parseInput ["quit"]				= Quit
parseInput s					= NAA $ unwords s

doAction :: Action -> Game ()
doAction (Goto dir)          = actGoto dir
doAction (Look Nothing)      = actLook
doAction (Look (Just at))    = actLookAt at
doAction Inventory           = actInventory
doAction (Pickup Nothing)    = lift $ putStrLn $ "Pickup what?"
doAction (Pickup (Just obj)) = actPickup obj
doAction (Drop Nothing)      = lift $ putStrLn $ "Drop what?"
doAction (Drop (Just obj))   = actDrop obj
doAction Quit                = lift $ throw (ErrorCall "Quit")
doAction (NAA "")            = return ()
doAction (NAA str)           = lift $ putStrLn $ "Don't know how to " ++ str ++ "."

actGoto :: Dir -> Game ()
actGoto dir = do
  state <- get
  room <- getRoom $ gsInRoom state
  case getExit dir room of
    Just new -> setRoom new
    Nothing -> lift $ putStrLn $ "There's no exit to the " ++ (show dir) ++ "."

actLook :: Game ()
actLook = do
  state <- get
  room <- getRoom $ gsInRoom state
  lift $ printRoom room{rVisited = False}
  printItemList "There is" $ rItems room
	
actLookAt :: String -> Game ()
actLookAt at = do
  state <- get
  room <- getRoom $ gsInRoom state
  if S.member at (gsInventory state)
    then do 
      item <- getItem at
      runEvent <- evalEvent EvLookAt (iEvents item)
      if runEvent == True
        then return ()
        else lift $ printItemInfo item --Default action
    else do
      if S.member at (rItems room)
        then do 
	  item <- getItem at
          runEvent <- evalEvent EvLookAt (iEvents item)
          if runEvent == True
            then return ()
            else lift $ printItemInfo item --Default action
	else lift $ putStrLn $ "Don't see " ++ at ++ " here."
	
actInventory :: Game ()
actInventory = do
  state <- get
  printItemList "You have" $ gsInventory state

actPickup :: String -> Game ()
actPickup obj = do
  state <- get
  room <- getRoom $ gsInRoom state
  if S.member obj (rItems room)
    then do
      let newRoomItems = S.delete obj (rItems room)
      let newInventory = S.insert obj (gsInventory state)
      updateRoom room{ rItems = newRoomItems }
      updateInventory newInventory
      item <- getItem obj
      lift $ putStrLn $ "Got " ++ (itemFullName item) ++ "."
    else lift $ putStrLn $ "I can't see " ++ obj ++ " here."

actDrop :: String -> Game ()
actDrop obj = do
  state <- get
  room <- getRoom $ gsInRoom state
  if S.member obj (gsInventory state)
    then do
      let newInventory = S.delete obj (gsInventory state)
      let newRoomItems = S.insert obj (rItems room)
      updateRoom room{ rItems = newRoomItems }
      updateInventory newInventory
      item <- getItem obj
      lift $ putStrLn $ "Dropped " ++ (itemFullName item) ++ "."
    else lift $ putStrLn $ "I don't have " ++ obj ++ "." 


