module GameState ( Dir(..)
                 , Room(..)
                 , Item(..)
                 , GameState(..)
                 , Game
                 , emptyGameState
                 , buildGame
                 , itemFullName
                 , getItemID
                 , getItem
                 , getRoomID
                 , getRoom
                 , setRoom
                 , updateRoom
                 , updateInventory
                 , getExit
                 , hasInventory
                 , itemInRoom
                 , printItemList
                 , printItemInfo
                 , printRoom
                 )
where


import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

data Dir = North
         | East
         | South
         | West
  deriving (Ord,Eq,Read,Show)

data Room = Room { rID :: String
                 , rTitle :: String
                 , rDesc :: String
                 , rExits :: M.Map Dir String
                 , rVisited :: Bool
                 , rItems :: S.Set String
                 }
                 deriving(Read)

data Item = Item { iID :: String
                 , iPre :: String
                 , iName :: String
                 , iDesc :: String
                 }
                 deriving(Read)
	
data GameState = GameState { gsRooms :: M.Map String Room 
                           , gsItems :: M.Map String Item
                           , gsInRoom :: String
                           , gsInventory :: S.Set String
                           }

type Game a = StateT GameState IO a

emptyGameState = GameState M.empty M.empty "" S.empty

buildGame :: [Room] -> [Item] -> Game ()
buildGame newRooms newItems = do
  state <- get
  let rooms = M.fromList [(getRoomID r,r) | r <- newRooms]
  let items = M.fromList [(getItemID i,i) | i <- newItems]
  put $ state{ gsRooms = rooms, gsItems = items }

itemFullName :: Item -> String
itemFullName (Item{ iPre = pre, iName = name}) = pre ++ " " ++ name

getItemID :: Item -> String
getItemID item = iID item

getItem :: String -> Game Item
getItem id = do
  state <- get
  case (M.lookup id (gsItems state) ) :: Maybe Item of
    Just item -> return item
    Nothing -> return undefined

getRoomID :: Room -> String
getRoomID room = rID room

getRoom :: String -> Game Room
getRoom id = do
  state <- get
  case (M.lookup id (gsRooms state) ) :: Maybe Room of
    Just room -> return room
    Nothing -> return undefined
		
setRoom :: String -> Game ()
setRoom id = do
  state <- get
  let newRoom = if (M.member id (gsRooms state)) then id else gsInRoom state
  put $ state{ gsInRoom = newRoom }

updateRoom :: Room -> Game ()
updateRoom r = let rid = getRoomID r in do
  state <- get
  let newrooms = M.adjust (const r) rid $ gsRooms state
  put $ state{ gsRooms = newrooms }

updateInventory :: S.Set String -> Game ()
updateInventory set = do
  state <- get
  put $ state{ gsInventory = set }

getExit :: Dir -> Room -> Maybe String
getExit dir room = M.lookup dir (rExits room) 

hasInventory :: String -> Game Bool
hasInventory id = do
  state <- get
  return $ S.member id $ gsInventory state
	
itemInRoom :: String -> Room -> Bool
itemInRoom id r = S.member id (rItems r)

printItemList :: String -> S.Set String -> Game ()
printItemList pre set = printItemList' pre $ S.elems set
  where
    printItemList' :: String -> [String] -> Game ()
    printItemList' _ [] = return ()
    printItemList' pre (l:ls) = do
      item <- getItem l
      lift $ putStrLn $ pre ++ " " ++ (itemFullName item) ++ "."
      printItemList' pre ls

printItemInfo :: Item -> IO ()
printItemInfo (Item{iDesc = desc}) = putStrLn $ desc

printRoom :: Room -> IO ()
printRoom (Room { rTitle = title, rDesc = desc, rVisited = visited}) = do
  putStrLn ""
  putStrLn title
  if visited
    then return ()
    else putStrLn desc

