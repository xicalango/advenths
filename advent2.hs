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
	deriving (Eq,Show)

data Action =
	  Goto Dir
	| Quit
	| Look --Maybe String
	| Inventory
	| Pickup --String
	| Drop --String
	| NAA String --Not an Action

data Room = Room { rID :: String
                 , rTitle :: String
                 , rDesc :: String
                 , rExits :: [Exit]
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

type Exit = (Dir,String)

items = [Item { iID = "cage"
              , iPre = "a little"
              , iName = "Cage"
              , iDesc = "A little cage"
              }
        ]

rooms = [Room { rID = "main"
              , rTitle = "Mainway"
              , rDesc = "This is the mainway.\nThere is a small passage to the north."
              , rExits = [(North,"back")]
              , rVisited = False 
              , rItems = S.fromList ["cage"]
              }
        ,Room { rID = "back"
              , rTitle = "Backway"
              , rDesc = "This is the backway.\nThere is a big passage to the south."
              , rExits = [(South,"main")]
              , rVisited = False
              , rItems = S.empty 
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

getExit :: Dir -> Room -> Maybe String
getExit dir room = 
	if hasDir room dir
	then Just $ getExit' dir $ rExits room
	else Nothing
	where
		getExit' :: Dir -> [Exit] -> String
		getExit' _ [] = ""
		getExit' dir (e:es)
			| fst e == dir = snd e
			| otherwise = getExit' dir es

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

hasDir :: Room -> Dir -> Bool
hasDir (Room {rExits = dirs}) dir = any (==dir) $ map fst dirs

parseInput :: String -> Action
parseInput "north"  = Goto North
parseInput "south"  = Goto South
parseInput "east"   = Goto East
parseInput "west"   = Goto West
parseInput "quit"   = Quit
parseInput "look"   = Look
parseInput "invent" = Inventory
parseInput "pickup" = Pickup
parseInput "drop"   = Drop
parseInput s        = NAA s

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

actInventory :: StateT GameState IO ()
actInventory = do
	state <- get
	printItemList "You have" $ gsInventory state

actPickup :: StateT GameState IO ()
actPickup = return () --TODO

actDrop :: StateT GameState IO ()
actDrop = return ()   --TODO

doAction :: Action -> StateT GameState IO ()
doAction (Goto dir) = actGoto dir
doAction Look       = actLook
doAction Inventory  = actInventory
doAction Pickup     = actPickup
doAction Drop       = actDrop
doAction Quit       = lift $ throw (ErrorCall "Quit")
doAction (NAA str)  = lift $ putStrLn $ "Don't know how to " ++ str ++ "."


playGame :: StateT GameState IO ()
playGame = do
	showCurrentRoom
	lift $ putStr "> "
	line <- lift $ getLine
	doAction $ parseInput line
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

