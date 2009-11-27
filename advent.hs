data Dir = 
	  North
	| East
	| South
	| West
	deriving (Eq,Show)

data Action =
	  Goto Dir
	| Quit
	| NAA String --Not a Action

type Exit = (Dir,Int)
type Room = (String,[Exit])

rooms :: [(Int,Room)]
rooms = [(0,
		("Mainway",[(North,1)])
	 )
	,(1,
		("Backway",[(South,0)])
	 )
	]

getRoom :: Int -> Room
getRoom = getRoom' rooms
	where
		getRoom' :: [(Int,Room)] -> Int -> Room
		getRoom' (x:xs) n
			| fst x == n = snd x
			| otherwise = getRoom' xs n
			
getRoomEx :: Room -> Dir -> Room
getRoomEx (_,dirs) = getRoomEx' dirs
	where
		getRoomEx' :: [Exit] -> Dir -> Room
		getRoomEx' (d:ds) dir
			| fst d == dir = getRoom $ snd d
			| otherwise = getRoomEx' ds dir


printRoom :: Room -> IO ()
printRoom (desc,exits) = do
	putStrLn ""
	putStrLn desc
	putStrLn "Exits:"
	printExits exits
	where
		printExits :: [Exit] -> IO ()
		printExits [] = return ()
		printExits (x:xs) = do
			putStrLn $ show $ fst x
			printExits xs

hasDir :: Room -> Dir -> Bool
hasDir (_,dirs) dir = any (==dir) $ map fst dirs

parseInput :: String -> Action
parseInput "north" = Goto North
parseInput "south" = Goto South
parseInput "east" = Goto East
parseInput "west" = Goto West
parseInput "quit" = Quit
parseInput s = NAA s

doAction :: Room -> Action -> IO ()
doAction room (Goto dir) = 
	if hasDir room dir
	then doRoom $ getRoomEx room dir 
	else do 
		putStrLn $ "No exit to " ++ (show dir) ++ "!"
		doRoom room

doAction room (NAA s) = do 
	putStrLn $ "Don't know to " ++ s ++ "!"
	doRoom room

doAction _ Quit = return ()

doRoom :: Room -> IO ()
doRoom room = do
	printRoom room
	putStr "> "
	x <- getLine
	doAction room $ parseInput x


main :: IO ()
main = do
	doRoom $ getRoom 0
	putStrLn "Bye!"
