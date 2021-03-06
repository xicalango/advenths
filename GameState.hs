module GameState ( Dir(..)
                 , Room(..)
                 , Item(..)
                 , GameState(..)
                 , Game
                 , Event(..)
                 , Command(..)
                 , Cond(..)
                 , Script
                 , EventHandlers
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
                 , runScript
                 , evalEvent
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
                 , iEvents :: EventHandlers
                 }
          deriving(Read)
	
data GameState = GameState { gsRooms :: M.Map String Room 
                           , gsItems :: M.Map String Item
                           , gsInRoom :: String
                           , gsInventory :: S.Set String
                           , gsVariables :: M.Map String String
                           }

data Event = EvLookAt
           | EvDrop
           | EvPickup
           | EvRead
           | EvMove
           deriving(Ord,Eq,Read)

data Command = IF Cond Script			-- If Condition is True run Script
             | IFELSE Cond Script Script	-- If Condition is True run first Script, else run second Script
             | OpenExit String (Dir,String)	-- In room named with the first string, open the Exit given by the Tuple
             | CloseExit String Dir		-- In room named by the first string, close the Exit to Dir
             | GetInventory String		-- Add the given Inventory object
             | LoseInventory String		-- Remove the given Inventory object
             | Message String			-- Print a message
             | SetVar String String		-- Give the variable ,named by first string, the value of the second string
             | ClearVar String			-- Clear the variable with the given name
             | PrintVar String			-- Print Variable given by the first string
             | PlaceItem String String		-- Put Item given by second string in room given by the first string
             deriving(Ord,Eq,Read)

data Cond = InRoom String			-- Checks if player is in the room given by string string
          | HasInventory String			-- Checks if player has the inventory item given by the string
          | VarSet String			-- Checks if the variable given by the string exists
          | VarEq String String			-- Checks if the value of the variable given by the fst string equals the snd string
          | And Cond Cond			-- Is true when both given conditions are true
          | Or Cond Cond			-- Is true when either one of the given conditions is true
          | Not Cond				-- Is true when the given condition is false
          deriving(Ord,Eq,Read)
          
type Script = [Command]

type EventHandlers = M.Map Event Script

type Game a = StateT GameState IO a

emptyGameState = GameState M.empty M.empty "" S.empty M.empty

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

inRoom :: String -> Game Bool
inRoom room = do
	state <- get
	return $ room == gsInRoom state

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

evalCondition :: Cond -> Game Bool
evalCondition (HasInventory inv) = hasInventory inv
evalCondition (InRoom room) = inRoom room

evalCondition (VarSet var) = do
  state <- get
  return $ M.member var (gsVariables state)

evalCondition (VarEq var val) = do
  state <- get
  case ( M.lookup var (gsVariables state) ) :: Maybe String of
    Just str -> return $ val == str
    Nothing -> return False

evalCondition (Or c1 c2) = do
  ec1 <- evalCondition c1
  ec2 <- evalCondition c2
  return $ ec1 `or` ec2
  where
   or :: Bool -> Bool -> Bool
   or True _ = True
   or _ True = True
   or _ _ = False

evalCondition (And c1 c2) = do
  ec1 <- evalCondition c1
  ec2 <- evalCondition c2
  return $ ec1 `and` ec2
  where
   and :: Bool -> Bool -> Bool
   and False _ = False
   and _ False = False
   and _ _ = True

evalCondition (Not c) = do
  ec <- evalCondition c
  return $ not ec
  where
   not :: Bool -> Bool
   not True = False
   not _ = True

evalCommand :: Command -> Game ()
evalCommand (IF cond scr) = do
	cstate <- evalCondition cond
	if cstate == True
	  then runScript scr
	  else return ()
	  
evalCommand (IFELSE cond scrtrue scrfalse) = do
	cstate <- evalCondition cond
	if cstate == True
	  then runScript scrtrue
	  else runScript scrfalse

evalCommand (OpenExit room exit) = cmdOpenExit room exit
evalCommand (CloseExit room dir) = cmdCloseExit room dir
evalCommand (GetInventory inv) = cmdGetInventory inv
evalCommand (LoseInventory inv) = cmdLoseInventory inv
evalCommand (Message msg) = lift $ putStrLn msg
evalCommand (SetVar var val) = cmdSetVar var val
evalCommand (ClearVar var) = cmdClearVar var
evalCommand (PrintVar var) = cmdPrintVar var
evalCommand (PlaceItem rid iid) = cmdPlaceItem rid iid

cmdOpenExit :: String -> (Dir,String) -> Game ()
cmdOpenExit r (dir,newExit) = do
  room <- getRoom r
  let newExits = M.insert dir newExit (rExits room)
  updateRoom room

cmdCloseExit :: String -> Dir -> Game ()
cmdCloseExit r dir = do
  room <- getRoom r
  let newExits = M.delete dir (rExits room)
  updateRoom room

cmdGetInventory :: String -> Game ()
cmdGetInventory inv = do
  state <- get
  let newInventory = S.insert inv (gsInventory state)
  updateInventory newInventory

cmdLoseInventory :: String -> Game ()
cmdLoseInventory inv = do
  state <- get
  let newInventory = S.delete inv (gsInventory state)
  updateInventory newInventory

cmdSetVar :: String -> String -> Game ()
cmdSetVar var val = do
  state <- get
  let newVars = if M.member var (gsVariables state) then M.adjust (const val) var (gsVariables state) else M.insert var val (gsVariables state)
  put $ state{gsVariables = newVars}

cmdClearVar :: String -> Game ()
cmdClearVar var = do
  state <- get
  let newVars = M.delete var (gsVariables state)
  put $ state{gsVariables = newVars}

cmdPrintVar :: String -> Game ()
cmdPrintVar var = do
  state <- get
  case ( M.lookup var (gsVariables state) ) :: Maybe String of
    Just val -> lift $ putStrLn val
    Nothing -> lift $ putStrLn $ "!!Error: Variable '" ++ var ++ "' unset!!"

cmdPlaceItem :: String -> String -> Game ()
cmdPlaceItem rid iid = do
  state <- get
  room <- getRoom rid
  let newRoomItems = S.insert iid (rItems room)
  updateRoom room{rItems = newRoomItems}

runScript :: Script -> Game ()
runScript [] = return ()
runScript (c:cs) = do
  evalCommand c
  runScript cs

evalEvent :: Event -> EventHandlers -> Game Bool
evalEvent event handlers = 
  case (M.lookup event handlers) :: Maybe Script of
    Just script -> do
      runScript script
      return True
    Nothing -> return False

