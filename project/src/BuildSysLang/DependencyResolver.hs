module BuildSysLang.DependencyResolver (build, buildDependencyGraph, topologicalSort, DependencyGraph(..), FreshnessCheck) where
import qualified Data.Map as Map
import Data.Map (Map)
import BuildSysLang.AST (Target, Rule (..), BuildFile (..), Command (..))
import Control.Monad.State (StateT, get, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Set as Set
import qualified Control.Monad
import qualified System.Process
import System.Directory ( doesFileExist, setModificationTime )
import Data.Time.Clock (getCurrentTime)
import System.Exit (ExitCode(..))
import Data.Set (Set)

data DependencyGraph = DependencyGraph {
    adjacencyGraph :: Map Target [Target],
    dependencyRule :: Map Target Rule
} deriving (Show, Eq)

data Color = White | Gray | Black deriving (Show, Eq)

data VisitState = VisitState {
    colors :: Map Target Color,
    ordering :: [Target], -- finished nodes
    path :: [Target] -- current Gray Stack
}

newtype Cycle = Cycle [Target] deriving Eq

instance Show Cycle where
    show (Cycle targets) = printCycle targets

printCycle :: [Target] -> String
printCycle [] = ""
printCycle [trgt] = show trgt
printCycle (trgt:rest) = show trgt ++ " -> " ++ printCycle rest

buildDependencyGraph :: BuildFile -> DependencyGraph
buildDependencyGraph (BuildFile rules) = DependencyGraph {
    adjacencyGraph = Map.union (Map.fromList (buildDependencyList rules)) (Map.fromList (fmap (\t -> (t, [])) (extractDependencies rules))),
    dependencyRule = Map.fromList (buildDependencyRule rules)
}

topologicalSort :: DependencyGraph -> Either Cycle [Target]
topologicalSort graph = let initialState = VisitState {
    colors   = Map.map (const White) (adjacencyGraph graph),
    ordering = [],
    path     = []
  } in case runStateT (mapM_ (visitNode graph) (Map.keys (adjacencyGraph graph))) initialState of
        Left cyc -> Left cyc
        Right (_, finalState) -> Right (reverse (ordering finalState))
-- runStateT action s0 :: Either Cycle ((), VisitState)

extractDependencies :: [Rule] -> [Target]
extractDependencies rules = rules >>= dependencies

buildDependencyList :: [Rule] -> [(Target, [Target])]
buildDependencyList = fmap (\rule -> (target rule, dependencies rule))

buildDependencyRule :: [Rule] -> [(Target, Rule)]
buildDependencyRule = fmap (\ rule -> (target rule, rule))

markVisited :: Target -> StateT VisitState (Either Cycle) ()
markVisited trgt = do
    modify (\visitState -> visitState {
        colors = Map.insert trgt Gray (colors visitState),
        path = trgt: path visitState
    })

markFinished :: Target -> StateT VisitState (Either Cycle) ()
markFinished trgt = do
    modify (\visitState -> visitState {
        colors = Map.insert trgt Black (colors visitState),
        ordering = trgt : ordering visitState,
        path = tail (path visitState)
    })

visitNode :: DependencyGraph -> Target -> StateT VisitState (Either Cycle) ()
visitNode graph trgt = do
    visitState <- get
    case Map.lookup trgt (colors visitState) of
        (Just White) -> do
            markVisited trgt
            case Map.lookup trgt (adjacencyGraph graph) of
                Nothing -> error "All targets must have dependencies declared"
                (Just deps) -> mapM_ (visitNode graph) deps --mapM_ sequences a list of monadic actions left-to-right, threading the state (and short-circuiting on the first Left) automatically 
            markFinished trgt
        (Just Gray) -> lift (Left (Cycle (buildCyclePath trgt (path visitState))))
        (Just Black) -> return ()
        _ -> error "All targets must be present"

buildCyclePath :: Target -> [Target] -> [Target]
buildCyclePath trgt pth = trgt : reverse (takeWhile (/= trgt) pth) ++ [trgt]

type FreshnessCheck = Target -> IO Bool -- a function which would determine whether a Target needs rebuilding

data BuildError = BuildError {
    failedTarget  :: Target,
    failedCommand :: Maybe Command,
    exitInfo      :: String
} deriving (Show)

type BuildResult = Either BuildError ()

build :: DependencyGraph -> FreshnessCheck -> IO Bool
build graph freshnessCheck =
  case topologicalSort graph of
    Left cyc  -> putStrLn ("Build failed: Circular dependency " ++ show cyc) >> return False
    Right topoOrder -> do
      (_, rebuildSet) <- runStateT (refreshGraph graph topoOrder freshnessCheck) Set.empty
      result <- rebuildGraph graph rebuildSet topoOrder
      case result of
        Right () -> putStrLn "Build succeeded" >> return True
        Left err -> (putStrLn $
            "Build failed: target " ++ failedTarget err
            ++ maybe "" (\command -> ", command " ++ show command) (failedCommand err)
            ++ ": " ++ exitInfo err) >> return False

-- a computation which tracks the set of targets that needs rebuilding
refreshGraph :: DependencyGraph -> [Target] -> FreshnessCheck -> StateT (Set Target) IO ()
refreshGraph _ [] _ = return ()
refreshGraph graph (dependency:targets) freshnessCheck = do
    rebuildState <- get
    needsRebuilding <- lift (freshnessCheck dependency)
    case Map.lookup dependency (adjacencyGraph graph) of
        Just deps -> Control.Monad.when (any (`Set.member` rebuildState) deps || needsRebuilding) $ modify (Set.insert dependency)
        Nothing -> return ()
    refreshGraph graph targets freshnessCheck

rebuildGraph :: DependencyGraph -> Set Target -> [Target] -> IO BuildResult
rebuildGraph _ _ [] = return (Right ())
rebuildGraph graph rebuildSet (trgt:rest) = do
    result <- if Set.member trgt rebuildSet
              then case Map.lookup trgt (dependencyRule graph) of
                       Just rule -> executeRecipe (recipe rule)
                       Nothing -> return (Right ())
              else return (Right ())
    case result of
        Right () -> rebuildGraph graph rebuildSet rest
        Left err -> return (Left (err { failedTarget = trgt }))

executeCommand :: Command -> IO BuildResult
executeCommand (Shell command) = do
    code <- System.Process.system command
    case code of
        ExitSuccess   -> return (Right ())
        ExitFailure n -> return (Left (BuildError {
            failedTarget  = "",
            failedCommand = Just (Shell command),
            exitInfo      = "exited with code " ++ show n
        }))
executeCommand (Touch pth) = do
    exists <- doesFileExist pth
    if exists
        then do 
            time <- getCurrentTime
            setModificationTime pth time
            return (Right ())
        else do
            writeFile pth ""
            return (Right ())
executeCommand (Echo command) = do
    putStrLn command
    return (Right ())

-- Executes a recipe in the terminal
executeRecipe :: [Command] -> IO BuildResult
executeRecipe [] = return (Right ())
executeRecipe (command:rest) = do
    result <- executeCommand command
    case result of
        Right () -> executeRecipe rest
        Left err -> return (Left err)

