module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Exception
import System.IO

data Dir = 
	  North
	| East
	| South
	| West
	deriving (Ord,Eq,Show)

data Action =
	  Goto Dir
	| Quit
	| Look (Maybe String)
	| Inventory
	| Pickup (Maybe String)
	| Drop (Maybe String)
	| NAA String --Not an Action

data Room = Room { rID :: String
                 , rTitle :: String
                 , rDesc :: String
                 , rExits :: M.Map Dir String
                 , rVisited :: Bool
                 , rItems :: S.Set String
                 }

data Item = Item { iID :: String
                 , iPre :: String
                 , iName :: String
                 , iDesc :: String
                 }
	
data GameState = GameState { gsRooms :: M.Map String Room 
                           , gsItems :: M.Map String Item
                           , gsInRoom :: String
                           , gsInventory :: S.Set String
                           }

items = [Item { iID = "cage"
              , iPre = "a little"
              , iName = "cage"
              , iDesc = "A little cage."
              }
        ,Item { iID = "rod"
              , iPre = "a"
              , iName = "rod"
              , iDesc = "A black and shiny rod."
              }
        ]

rooms = [Room { rID = "main"
              , rTitle = "Mainway"
              , rDesc = "This is the mainway.\nThere is a small passage to the north."
              , rExits = M.fromList [(North,"back")]
              , rVisited = False 
              , rItems = S.fromList ["cage"]
              }
        ,Room { rID = "back"
              , rTitle = "Backway"
              , rDesc = "This is the backway.\nThere is a big passage to the south.\nAlso you can go east an west."
              , rExits = M.fromList [(South,"main"),(East,"beast"),(West,"best")]
              , rVisited = False
              , rItems = S.empty 
              }
        ,Room { rID = "beast"
              , rTitle = "Back East"
              , rDesc = "You have come to the beast!"
              , rExits = M.fromList [(South,"back")]
              , rVisited = False
              , rItems = S.empty 
              }
        ,Room { rID = "best"
              , rTitle = "Back West"
              , rDesc = "This is the west."
              , rExits = M.fromList [(South,"back")]
              , rVisited = False
              , rItems = S.fromList ["rod"]
              }
        ]


buildGame :: [Room] -> [Item] -> StateT GameState IO ()
buildGame newRooms newItems = do
	state <- get
	let rooms = M.fromList [(getRoomID r,r) | r <- newRooms]
	let items = M.fromList [(getItemID i,i) | i <- newItems]
	put $ state{ gsRooms = rooms, gsItems = items }
	          
getItemID :: Item -> String
getItemID item = iID item

getItem :: String -> StateT GameState IO Item
getItem id = do
	state <- get
	case (M.lookup id (gsItems state) ) :: Maybe Item of
		Just item -> return item
		Nothing -> return undefined

getRoomID :: Room -> String
getRoomID room = rID room

getRoom :: String -> StateT GameState IO Room
getRoom id = do
	state <- get
	case (M.lookup id (gsRooms state) ) :: Maybe Room of
		Just room -> return room
		Nothing -> return undefined
		
setRoom :: String -> StateT GameState IO ()
setRoom id = do
	state <- get
	let newRoom = if (M.member id (gsRooms state) ) 
		then id
		else gsInRoom state
	put $ state{ gsInRoom = newRoom }

updateRoom :: Room -> StateT GameState IO ()
updateRoom r = let rid = getRoomID r in do
	state <- get
	let newrooms = M.adjust (const r) rid $ gsRooms state
	put $ state{ gsRooms = newrooms }

updateInventory :: S.Set String -> StateT GameState IO ()
updateInventory set = do
	state <- get
	put $ state{ gsInventory = set }

getExit :: Dir -> Room -> Maybe String
getExit dir room = M.lookup dir (rExits room) 

printItemList :: String -> S.Set String -> StateT GameState IO ()
printItemList pre set = printItemList' pre $ S.elems set
	where
		printItemList' :: String -> [String] -> StateT GameState IO ()
		printItemList' _ [] = return ()
		printItemList' pre (l:ls) = do
			item <- getItem l
			lift $ putStrLn $ pre ++ " " ++ (iPre item) ++ " " ++ (iName item) ++ "."
			printItemList' pre ls

printRoom :: Room -> IO ()
printRoom (Room { rTitle = title, rDesc = desc, rVisited = visited}) = do
	putStrLn ""
	putStrLn title
	if visited
		then return ()
		else putStrLn desc

hasInventory :: String -> StateT GameState IO Bool
hasInventory id = do
	state <- get
	return $ S.member id $ gsInventory state
	
itemInRoom :: String -> Room -> Bool
itemInRoom id r = S.member id (rItems r)

parseInput :: [String] -> Action
parseInput ["north"]       = Goto North
parseInput ["south"]       = Goto South
parseInput ["east"]        = Goto East
parseInput ["west"]        = Goto West
parseInput ["quit"]        = Quit
parseInput ["look"]        = Look Nothing
parseInput ("look":"at":os)   = Look $ Just $ unwords os
parseInput ("look":os)   = Look $ Just $ unwords os
parseInput ["invent"]      = Inventory
parseInput ["pickup"]      = Pickup Nothing
parseInput ("pickup":os) = Pickup $ Just $ unwords os
parseInput ["get"]      = Pickup Nothing
parseInput ("get":os) = Pickup $ Just $ unwords os
parseInput ["drop"]        = Drop Nothing
parseInput ("drop":os)   = Drop $ Just $ unwords os
parseInput s               = NAA $ unwords s

actGoto :: Dir -> StateT GameState IO ()
actGoto dir = do
	state <- get
	room <- getRoom $ gsInRoom state
	case getExit dir room of
		Just new -> setRoom new
		Nothing -> lift $ putStrLn $ "There's no exit to the " ++ (show dir) ++ "."

actLook :: StateT GameState IO ()
actLook = do
	state <- get
	room <- getRoom $ gsInRoom state
	lift $ printRoom room{rVisited = False}
	printItemList "There is" $ rItems room
	
printItemInfo :: Item -> IO ()
printItemInfo (Item{iDesc = desc}) = do
	putStrLn $ desc

actLookAt :: String -> StateT GameState IO ()
actLookAt at = do
	state <- get
	room <- getRoom $ gsInRoom state
	case S.member at (gsInventory state) of
		True -> do 
			item <- getItem at
			lift $ printItemInfo item
		False -> do
			case S.member at (rItems room) of
				True -> do 
					item <- getItem at
					lift $ printItemInfo item
				False -> lift $ putStrLn $ "Don't see " ++ at ++ " here."
	

actInventory :: StateT GameState IO ()
actInventory = do
	state <- get
	printItemList "You have" $ gsInventory state

actPickup :: String -> StateT GameState IO ()
actPickup obj = do
	state <- get
	room <- getRoom $ gsInRoom state
	case S.member obj (rItems room) of
		True -> do
			let newRoomItems = S.delete obj (rItems room)
			let newInventory = S.insert obj (gsInventory state)
			updateRoom room{ rItems = newRoomItems }
			updateInventory newInventory
		False -> lift $ putStrLn $ "I can't see " ++ obj ++ " here."

actDrop :: String -> StateT GameState IO ()
actDrop obj = do
	state <- get
	room <- getRoom $ gsInRoom state
	case S.member obj (gsInventory state) of
		True -> do
			let newInventory = S.delete obj (gsInventory state)
			let newRoomItems = S.insert obj (rItems room)
			updateRoom room{ rItems = newRoomItems }
			updateInventory newInventory
		False -> lift $ putStrLn $ "I don't have " ++ obj ++ "." 

doAction :: Action -> StateT GameState IO ()
doAction (Goto dir)   = actGoto dir
doAction (Look Nothing) = actLook
doAction (Look (Just at)) = actLookAt at
doAction Inventory    = actInventory
doAction (Pickup Nothing)  = lift $ putStrLn $ "Pickup what?"
doAction (Pickup (Just obj)) = actPickup obj
doAction (Drop Nothing)    = lift $ putStrLn $ "Drop what?"
doAction (Drop (Just obj))   = actDrop obj
doAction Quit         = lift $ throw (ErrorCall "Quit")
doAction (NAA str)    = lift $ putStrLn $ "Don't know how to " ++ str ++ "."

playGame :: StateT GameState IO ()
playGame = do
	showCurrentRoom
	lift $ putStr "> "
	line <- lift $ getLine
	doAction $ parseInput $ words line
	where
		showCurrentRoom :: StateT GameState IO ()
		showCurrentRoom = do
			state <- get
			room <- getRoom $ gsInRoom state
			lift $ printRoom room
			printItemList "There is" $ rItems room
			case rVisited room of
				True -> return()
				False -> updateRoom room{ rVisited=True }

		
doGame :: StateT GameState IO ()
doGame = do
	setupGame
	forever $ playGame
	return ()
	where
		setupGame = do
			buildGame rooms items
			setRoom "main"

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	evalStateT (doGame) $ GameState M.empty M.empty "" S.empty
--	doRoom $ getRoom "main"
--	putStrLn "Bye!"

