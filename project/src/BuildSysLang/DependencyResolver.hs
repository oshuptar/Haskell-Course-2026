module BuildSysLang.DependencyResolver (build, buildDependencyGraph, topologicalSort) where
import qualified Data.Map as Map
import Data.Map (Map)
import BuildSysLang.AST (Target, Rule (..), BuildFile (..), Command (..))
import Control.Monad.State (StateT, get, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Set as Set
import qualified Control.Monad
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

build :: DependencyGraph -> FreshnessCheck -> IO ()
build graph freshnessCheck =
  case topologicalSort graph of
    Left cycle  -> putStrLn $ "Build failed: Circular dependency " ++ show cycle -- report the cycle, abort
    Right topoOrder -> do
      (_, rebuildSet) <- runStateT (refreshGraph graph topoOrder freshnessCheck) Set.empty
      success <- rebuildGraph graph rebuildSet topoOrder
      unless success $ putStrLn "Build failed: a recipe errored"

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

rebuildGraph :: DependencyGraph -> Set Target -> [Target] -> IO Bool
rebuildGraph graph rebuildSet [] = return True
rebuildGraph graph rebuildSet (target:rest) = do
    result <- if Set.member target rebuildSet
              then case Map.lookup target (dependencyRule graph) of
                       Just rule -> executeRecipe (recipe rule)
                       Nothing -> return True
              else return True
    if result then rebuildGraph graph rebuildSet rest else return False

executeCommand :: Command -> IO Bool
executeCommand command = do
    undefined
    -- parse and execute the command here

-- Executes a recipe in the terminal
executeRecipe :: [Command] -> IO Bool
executeRecipe [] = return True
executeRecipe (command:rest) = do
    result <- executeCommand command
    if result then executeRecipe rest else return False
