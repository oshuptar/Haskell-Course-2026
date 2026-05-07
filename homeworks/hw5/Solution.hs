module Solution(main) where
import Control.Monad.State
import Data.Map
import Distribution.Compat.Prelude (readMaybe)
import Data.List (intercalate)
import Data.Char (intToDigit)

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

execInstr :: Instr -> State [Int] ()
execInstr (PUSH val) = do modify (val :)
execInstr POP = do
    stack <- get
    case stack of
        [] -> return ()
        x:xs -> put xs
execInstr DUP = do
    stack <- get
    case stack of
        [] -> return ()
        x:xs -> modify (x:)
execInstr SWAP = do
    stack <- get
    case stack of
        x1:x2:xs -> put (x2:x1:xs)
        _ -> return ()
execInstr ADD = do
    stack <- get
    case stack of
        x1:x2:xs -> put ((x1 + x2) : xs)
        _ -> return ()
execInstr MUL = do
    stack <- get
    case stack of
        x1:x2:xs -> put ((x1 * x2) : xs)
        _ -> return ()
execInstr NEG = do
    stack <- get
    case stack of
        []   -> return ()
        x:xs -> put ((-x) : xs)

execProg :: [Instr] -> State [Int] ()
execProg [] = return ()
execProg (head:remaining) = do
    execInstr head
    execProg remaining

runProg :: [Instr] -> [Int]
runProg instructions = let (res, state) = runState (execProg instructions) [] in state

-- 2:
data Expr = Num Int | Var String | Add Expr Expr | Mul Expr Expr | Neg Expr | Assign String Expr | Seq Expr Expr
eval :: Expr -> State (Map String Int) Int
eval (Num !val) = return val
eval (Var variable) = do
    map <- get
    case Data.Map.lookup variable map of
        Just val -> return val
eval (Add !exprL !exprR) = do
    resultL <- eval exprL
    resultR <- eval exprR
    return (resultL + resultR)
eval (Mul !exprL !exprR) = do
    resultL <- eval exprL
    resultR <- eval exprR
    return (resultL * resultR)
eval (Neg !expr) = do
    result <- eval expr
    return (-result)
eval (Assign var expr) = do
    result <- eval expr
    modify (Data.Map.insert var result)
    return result
eval (Seq exprL exprR) = do
    eval exprL
    eval exprR

runEval :: Expr -> Int
runEval expression = let (res,state) = runState (eval expression) Data.Map.empty in res

--3:
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM left right i 0 = do
    cache <- get
    modify (Data.Map.insert (i, 0) i)
    return i
editDistM left right 0 j = do
    cache <- get
    modify (Data.Map.insert (0, j) j)
    return j
editDistM xs ys i j = do
    cache <- get
    case Data.Map.lookup (i, j) cache of
        Just dist -> return dist
        Nothing ->
            if xs !! (i-1) == ys !! (j-1)
                then do
                    result <- editDistM xs ys (i-1) (j-1)
                    modify (Data.Map.insert (i, j) result)
                    return result
                else do
                    del <- editDistM xs ys (i-1) j
                    ins <- editDistM xs ys i (j-1)
                    sub <- editDistM xs ys (i-1) (j-1)
                    let result = 1 + minimum [del, ins, sub]
                    modify (Data.Map.insert (i, j) result)
                    return result

editDistance :: String -> String -> Int
editDistance left right = let (res, state) =
                                runState (editDistM left right (length left) (length right)) Data.Map.empty
                            in res

testEditDistance :: String -> String -> Int -> (Bool, Int)
testEditDistance left right expected = let result = editDistance left right in (result == expected, result)

-- 4:

-- TODO: keep track of exhausted players, once a player is exhausted - mark him
newtype Position = Position (Int, Int) deriving (Show, Eq, Ord)

data Tile = TileInfo {
    nextTile :: Maybe Position,
    tileType :: TileType
}

data TileType = Treasure Int | Trap Int | DecisionTile (Map String Position) | Obstacle Effect | FinalTile | Empty
data Effect = Delay Int | LoseEnergy Int

type Maze = Map Position Tile

data PlayerState = PlayerState {
    position :: Position,
    energyPoints :: Int,
    treasurePoints :: Int,
    delayTime :: Int
}

data GameState = GameState {
    maze :: Maze,
    activePlayers :: Int,
    playerTurn :: Int, -- the Id of the player whose turn it is to make a move
    players :: Map Int PlayerState,
    winner :: Maybe Int,
    defaultEnergyPoints :: Int,
    startPosition :: Position
}

instance Show GameState where
    show gameState = undefined

type AdventureGame a = StateT GameState IO a

advanceTurn :: AdventureGame ()
advanceTurn = do
    modify (\gameState -> gameState {playerTurn = (playerTurn gameState + 1) `mod` activePlayers gameState})

getCurrentPlayer :: AdventureGame (Int, PlayerState)
getCurrentPlayer = do
    gameState <- get
    let playerId = playerTurn gameState
    case Data.Map.lookup playerId (players gameState) of
        Just playerState -> return (playerId, playerState)
        Nothing -> error ("Player not found: " ++ show playerId)

updateCurrentPlayer :: (PlayerState -> PlayerState) -> AdventureGame ()
updateCurrentPlayer update = do
    gameState <- get
    let playerId = playerTurn gameState
    modify (\gameState -> gameState { players = Data.Map.adjust update playerId (players gameState) })

handleDelay :: AdventureGame Bool
handleDelay = do
    (_, playerState) <- getCurrentPlayer
    if delayTime playerState > 0
        then do
            updateCurrentPlayer $ \p -> p { delayTime = delayTime p - 1 }
            liftIO $ putStrLn ("The player is delayed for " ++ show (delayTime playerState) ++ " more rounds.")
            return True
        else
            return False

getTile :: Position -> AdventureGame (Maybe Tile)
getTile position = do
    gameState <- get
    return (Data.Map.lookup position (maze gameState))

handleLocation :: AdventureGame Bool
handleLocation = do
    (playerId, playerState) <- getCurrentPlayer
    let currentPosition = position playerState
    foundTile <- getTile currentPosition
    let currentTile = case foundTile of
            Just tile -> tile
            Nothing -> error ("Player " ++ show playerId ++ " appeared outside of maze")
    case tileType currentTile of
        FinalTile -> return True
        Empty -> return False
        Obstacle (Delay val) -> do
            updateCurrentPlayer (\playerState -> playerState {delayTime = val})
            return False
        Treasure val -> do
            updateCurrentPlayer (\playerState -> playerState {treasurePoints = treasurePoints playerState + val})
            return False
        Obstacle (LoseEnergy val) -> do
            updateCurrentPlayer (\playerState -> playerState {energyPoints = energyPoints playerState - val})
            return False
        Trap val -> do
            updateCurrentPlayer (\playerState -> playerState {treasurePoints = treasurePoints playerState - val})
            return False
        _ -> return False


-- removePlayer :: Int -> AdventureGame ()
-- removePlayer playerId = do
--     gameState <- get
--     modify (\state -> state {
--         activePlayers = activePlayers state - 1,
--         players = Data.Map.delete playerId (players state)
--     })

addPlayer :: AdventureGame ()
addPlayer = do
    gameState <- get
    modify (\state -> state {
        activePlayers = activePlayers state + 1,
        players = Data.Map.insert (activePlayers state) PlayerState {
            position = startPosition gameState,
            energyPoints = defaultEnergyPoints gameState,
            treasurePoints = 0,
            delayTime = 0
        } (players state)
    })

moveOneStep :: AdventureGame Bool
moveOneStep = do
    (playerId, playerState) <- getCurrentPlayer
    if energyPoints playerState <= 0
    then do
        lift $ putStrLn ("Player " ++ show playerId ++ " is exhausted")
        return False
    else do
        foundTile <- getTile (position playerState)
        nextPosition <- case foundTile of
            Nothing -> error ("Player " ++ show playerId ++ " appeared outside of maze")
            Just tile ->
                case tileType tile of
                    DecisionTile options -> do
                        choice <- makeDecision (Data.Map.keys options)
                        case Data.Map.lookup choice options of
                            Just pos -> return pos
                            Nothing  -> error ("Invalid decision: " ++ choice)
                    _ -> case nextTile tile of
                            Just pos -> return pos
                            Nothing -> do
                                lift $ putStrLn "There is no next tile. Player cannot move"
                                return (position playerState)

        updateCurrentPlayer (\playerState ->
            playerState {energyPoints = energyPoints playerState - 1,
            position = nextPosition
        })
        return True


moveSteps :: Int -> AdventureGame Int
moveSteps 0 = return 0
moveSteps steps = do
    didMove <- moveOneStep
    if not didMove
        then return 0
    else do
        reachedGoal <- handleLocation
        if reachedGoal
        then do
            (playerId, _) <- getCurrentPlayer
            modify $ \gameState -> gameState { winner = Just playerId }
            lift $ putStrLn ("Player " ++ show playerId ++ " reached the final treasure!")
            return 1
        else do
            rest <- moveSteps (steps - 1)
            return (1 + rest)

movePlayer :: Int -> AdventureGame Int
movePlayer n
    | n <= 0 = return 0
    | n > 6 = return 0
    | otherwise = do
        isDelayed <- handleDelay
        if isDelayed then return 0
        else moveSteps n

getPlayerChoice  :: [String] -> IO String
getPlayerChoice options = do
    print options
    getLine

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    choice <- lift $ getPlayerChoice options
    if any (== choice) options
    then return choice
    else do
        lift $ putStrLn ("Option: " ++ show choice ++ " does not exist")
        makeDecision options

getDiceRoll :: IO Int
getDiceRoll = do
    putStrLn "Enter a dice roll (1-6): "
    input <- getLine
    case readMaybe input of
        Just n 
            | n >= 1 && n <= 6 -> return n
            | otherwise -> do
                putStrLn "Dice roll must be between 1 and 6"
                getDiceRoll
        Nothing -> do
            putStrLn "Please enter a valid integer"
            getDiceRoll

isGameOver :: AdventureGame Bool
isGameOver = do
    gameState <- get
    return (winner gameState /= Nothing || activePlayers gameState <= 0)

playTurn :: AdventureGame Bool
playTurn = do
    gameState <- get
    liftIO $ displayGameState gameState
    
    dice <- lift getDiceRoll
    moved <- movePlayer dice
    lift $ putStrLn ("Moved " ++ show moved ++ " spaces.")
    gameOver <- isGameOver
    if gameOver then return True
    else do
        return False

playRound :: Int -> AdventureGame Bool
playRound 0 = return False
playRound n = do
    ended <- playTurn
    if ended then return True
    else do advanceTurn
            playRound (n - 1)

getNumberOfPlayers :: IO Int
getNumberOfPlayers = do
    putStrLn "Enter number of players: "
    input <- getLine
    case readMaybe input of
        Just n 
            | n >= 1 -> return n
            | otherwise -> do
                putStrLn "Enter a positive number"
                getNumberOfPlayers
        Nothing -> do
            putStrLn "Please enter a valid integer"
            getNumberOfPlayers

addPlayers :: Int -> AdventureGame ()
addPlayers 0 = return ()
addPlayers n = do
    addPlayer
    addPlayers (n - 1)

playGame :: AdventureGame ()
playGame = do
    gameState <- get
    ended <- playRound (activePlayers gameState)
    if ended then do
            finalState <- get
            case winner finalState of
                Just winnerId -> liftIO $ putStrLn ("Congratulations to player: " ++ show winnerId)
                Nothing -> liftIO $ putStrLn "Game over. No winner."
    else playGame

startGame :: AdventureGame()
startGame = do
    numberOfPlayers <- liftIO getNumberOfPlayers
    addPlayers numberOfPlayers
    playGame

-- AI-generated version
displayGameState :: GameState -> IO ()
displayGameState gameState = do
    putStrLn "\n================ Treasure Hunters ================"
    putStrLn ("Current turn: Player " ++ show (playerTurn gameState))
    putStrLn ("Winner: " ++ showWinner (winner gameState))
    putStrLn "--------------------------------------------------"
    displayLegend
    putStrLn "--------------------------------------------------"
    putStrLn "Maze:"
    putStrLn (drawMaze gameState)
    putStrLn "--------------------------------------------------"
    putStrLn "Players:"
    mapM_ (putStrLn . showPlayer) (Data.Map.toList (players gameState))
    putStrLn "=================================================="

showWinner :: Maybe Int -> String
showWinner Nothing = "None"
showWinner (Just playerId) = "Player " ++ show playerId

showPosition :: Position -> String
showPosition (Position (x, y)) =
    "(" ++ show x ++ ", " ++ show y ++ ")"

showPlayer :: (Int, PlayerState) -> String
showPlayer (playerId, playerState) =
    "Player " ++ show playerId
    ++ " | position: " ++ showPosition (position playerState)
    ++ " | energy: " ++ show (energyPoints playerState)
    ++ " | treasure: " ++ show (treasurePoints playerState)
    ++ " | delay: " ++ show (delayTime playerState)

drawMaze :: GameState -> String
drawMaze gameState =
    unlines [ drawRow y | y <- reverse [minY .. maxY] ]
  where
    positions :: [Position]
    positions = Data.Map.keys (maze gameState)

    coordinates :: [(Int, Int)]
    coordinates = [ (x, y) | Position (x, y) <- positions ]

    xs = Prelude.map fst coordinates
    ys = Prelude.map snd coordinates

    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys

    drawRow :: Int -> String
    drawRow y =
        concat [ drawCell (Position (x, y)) | x <- [minX .. maxX] ]

    drawCell :: Position -> String
    drawCell pos =
        " " ++ [cellChar pos] ++ " "

    cellChar :: Position -> Char
    cellChar pos =
        case playersAt pos of
            []          -> tileChar pos
            [playerId]  -> playerChar playerId
            _           -> '*'

    playersAt :: Position -> [Int]
    playersAt pos =
        [ playerId
        | (playerId, playerState) <- Data.Map.toList (players gameState)
        , position playerState == pos
        ]

    playerChar :: Int -> Char
    playerChar playerId
        | playerId >= 0 && playerId <= 9 = intToDigit playerId
        | otherwise = 'P'

    tileChar :: Position -> Char
    tileChar pos
        | pos == startPosition gameState = 'S'
        | otherwise =
            case Data.Map.lookup pos (maze gameState) of
                Nothing -> ' '
                Just tile ->
                    case tileType tile of
                        Empty              -> '.'
                        Treasure _         -> '$'
                        Trap _             -> '!'
                        DecisionTile _     -> '?'
                        Obstacle _         -> '#'
                        FinalTile          -> 'F'

displayLegend :: IO ()
displayLegend = do
    putStrLn "Legend:"
    putStrLn "S = start"
    putStrLn "F = final treasure"
    putStrLn "$ = intermediate treasure"
    putStrLn "! = trap"
    putStrLn "? = decision point"
    putStrLn "# = obstacle"
    putStrLn ". = empty tile"
    putStrLn "0-9 = player position"
    putStrLn "* = multiple players on same tile"

main :: IO ()
main = do
    putStrLn "Task1"
    let testProgram = [PUSH 3, PUSH 4, ADD, DUP, PUSH 2, MUL, SWAP, NEG, POP]
    let expected = [14]
    print $ runProg testProgram
    print $ runProg testProgram == expected

    let skipProgram = [POP, DUP, SWAP, ADD, MUL, NEG, PUSH 10, SWAP, ADD, MUL, PUSH 5, SWAP]
    let expected = [10,5]
    print $ runProg skipProgram
    print $ runProg skipProgram == expected

    putStrLn "\nTask2"
    let testEval = Seq (Assign "x" (Num 10)) (Seq (Assign "y" (Add (Var "x") (Num 5))) (Seq (Assign "z" (Mul (Var "y") (Neg (Num 2)))) (Add (Var "z") (Var "x"))))
    let expected = -20
    print $ runEval testEval
    print $ runEval testEval == expected

    let testEval = Seq (Assign "a" (Num 4)) (Seq (Assign "b" (Mul (Var "a") (Num 3))) (Seq (Assign "a" (Add (Var "b") (Neg (Var "a")))) (Mul (Var "a") (Add (Var "b") (Num 1)))))
    let expected = 104
    print $ runEval testEval
    print $ runEval testEval == expected

    putStrLn "\nTask3"
    print $ testEditDistance "" "" 0
    print $ testEditDistance "" "abc" 3
    print $ testEditDistance "cat" "cut" 1
    print $ testEditDistance "cat" "cart" 1
    print $ testEditDistance "haskell" "haskell" 0
    print $ testEditDistance "kitten" "sitting" 3
    print $ testEditDistance "flaw" "lawn" 2
    print $ testEditDistance "intention" "execution" 5

    putStrLn "\nTask4"
    -- AI-generated Maze
    let adventureMaze :: Maze = Data.Map.fromList[
            (Position (0, 0), TileInfo (Just (Position (1, 0))) Empty),
            (Position (1, 0), TileInfo (Just (Position (2, 0))) (Treasure 5)),
            (Position (2, 0), TileInfo Nothing
            (DecisionTile (Data.Map.fromList
                [ ("forest", Position (3, 1))
                , ("cave", Position (3, -1))
                ]))),
            (Position (3, 1), TileInfo (Just (Position (4, 1))) Empty),
            (Position (4, 1), TileInfo (Just (Position (5, 1))) (Treasure 10)),
            (Position (5, 1), TileInfo (Just (Position (6, 1))) (Obstacle (Delay 1))),
            (Position (6, 1), TileInfo (Just (Position (7, 0))) Empty),
            (Position (3, -1), TileInfo (Just (Position (4, -1))) (Obstacle (LoseEnergy 2))),
            (Position (4, -1), TileInfo (Just (Position (5, -1))) (Trap 8)),
            (Position (5, -1), TileInfo (Just (Position (7, 0))) (Treasure 20)),
            (Position (7, 0), TileInfo Nothing
            (DecisionTile (Data.Map.fromList
                [ ("bridge", Position (8, 1))
                , ("tunnel", Position (8, -1))
                ]))),
            (Position (8, 1), TileInfo (Just (Position (9, 1))) (Trap 10)),
            (Position (9, 1), TileInfo (Just (Position (10, 0))) Empty),
            (Position (8, -1), TileInfo (Just (Position (9, -1))) (Obstacle (Delay 2))),
            (Position (9, -1), TileInfo (Just (Position (10, -1))) (Treasure 15)),
            (Position (10, -1), TileInfo (Just (Position (10, 0))) Empty),
            (Position (10, 0), TileInfo Nothing FinalTile)]
    let initialGameState :: GameState = GameState {
        maze = adventureMaze,
        activePlayers = 0,
        playerTurn = 0,
        players = Data.Map.empty,
        winner = Nothing,
        defaultEnergyPoints = 10,
        startPosition = Position (0,0)
    }
    _ <- runStateT startGame initialGameState
    return ()
