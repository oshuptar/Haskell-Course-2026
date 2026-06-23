module BuildSysLang.DependencyResolver (build, buildDependencyGraph, topologicalSort, DependencyGraph(..), FreshnessCheck, BuildResult(..)) where
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

newtype Cycle = Cycle [Target] deriving (Show, Eq)

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
        Left cycle -> Left cycle
        Right (_, finalState) -> Right (reverse (ordering finalState))
-- runStateT action s0 :: Either Cycle ((), VisitState)

extractDependencies :: [Rule] -> [Target]
extractDependencies rules = rules >>= dependencies

buildDependencyList :: [Rule] -> [(Target, [Target])]
buildDependencyList = fmap (\rule -> (target rule, dependencies rule))

buildDependencyRule :: [Rule] -> [(Target, Rule)]
buildDependencyRule = fmap (\ rule -> (target rule, rule))

markVisited :: Target -> StateT VisitState (Either Cycle) ()
markVisited target = do
    modify (\visitState -> visitState {
        colors = Map.insert target Gray (colors visitState),
        path = target: path visitState
    })

markFinished :: Target -> StateT VisitState (Either Cycle) ()
markFinished target = do
    modify (\visitState -> visitState {
        colors = Map.insert target Black (colors visitState),
        ordering = target : ordering visitState,
        path = tail (path visitState)
    })

visitNode :: DependencyGraph -> Target -> StateT VisitState (Either Cycle) ()
visitNode graph target = do
    visitState <- get
    case Map.lookup target (colors visitState) of
        (Just White) -> do
            markVisited target
            case Map.lookup target (adjacencyGraph graph) of
                Nothing -> error "All targets must have dependencies declared"
                (Just dependencies) -> mapM_ (visitNode graph) dependencies --mapM_ sequences a list of monadic actions left-to-right, threading the state (and short-circuiting on the first Left) automatically 
            markFinished target
        (Just Gray) -> lift (Left (Cycle (buildCyclePath target (path visitState))))
        (Just Black) -> return ()
        _ -> error "All targets must be present"

buildCyclePath :: Target -> [Target] -> [Target]
buildCyclePath target path = reverse (takeWhile (/= target) path) ++ [target]

type FreshnessCheck = Target -> IO Bool -- a function which would determine whether a Target needs rebuilding

data BuildError = BuildError {
    failedTarget  :: Target,
    failedCommand :: Maybe Command,
    exitInfo      :: String
} deriving (Show)

type BuildResult = Either BuildError ()

build :: DependencyGraph -> FreshnessCheck -> IO ()
build graph freshnessCheck =
  case topologicalSort graph of
    Left cycle  -> putStrLn ("Build failed: Circular dependency " ++ show cycle)
    Right topoOrder -> do
      (_, rebuildSet) <- runStateT (refreshGraph graph topoOrder freshnessCheck) Set.empty
      result <- rebuildGraph graph rebuildSet topoOrder
      case result of
        Right () -> putStrLn "Build succeeded"
        Left err -> putStrLn $
            "Build failed: target " ++ failedTarget err
            ++ maybe "" (\command -> ", command " ++ show command) (failedCommand err)
            ++ ": " ++ exitInfo err

-- a computation which tracks the set of targets that needs rebuilding
refreshGraph :: DependencyGraph -> [Target] -> FreshnessCheck -> StateT (Set Target) IO ()
refreshGraph graph [] freshnessCheck = return ()
refreshGraph graph (dependency:targets) freshnessCheck = do
    rebuildState <- get
    needsRebuilding <- lift (freshnessCheck dependency)
    case Map.lookup dependency (adjacencyGraph graph) of
        Just dependencies -> Control.Monad.when (any (`Set.member` rebuildState) dependencies || needsRebuilding) $ modify (Set.insert dependency)
        Nothing -> return ()
    refreshGraph graph targets freshnessCheck

rebuildGraph :: DependencyGraph -> Set Target -> [Target] -> IO BuildResult
rebuildGraph graph rebuildSet [] = return (Right ())
rebuildGraph graph rebuildSet (target:rest) = do
    result <- if Set.member target rebuildSet
              then case Map.lookup target (dependencyRule graph) of
                       Just rule -> executeRecipe (recipe rule)
                       Nothing -> return (Right ())
              else return (Right ())
    case result of
        Right () -> rebuildGraph graph rebuildSet rest
        Left err -> return (Left (err { failedTarget = target }))

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
executeCommand (Touch path) = do
    exists <- doesFileExist path
    if exists
        then do 
            time <- getCurrentTime
            setModificationTime path time
            return (Right ())
        else do
            writeFile path ""
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

