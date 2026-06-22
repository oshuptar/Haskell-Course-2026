module BuildSysLang.DependencyResolver where
import qualified Data.Map as Map
import Data.Map (Map)
import BuildSysLang.AST (Target, Rule (..), BuildFile (..), Command (..))
import Control.Monad.State (StateT, get, modify, runStateT, evalStateT)
import Control.Monad.Trans.Class (lift)

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

extractDependencies :: [Rule] -> [Target]
extractDependencies rules = rules >>= dependencies

buildDependencyList :: [Rule] -> [(Target, [Target])]
buildDependencyList = fmap (\rule -> (target rule, dependencies rule))

buildDependencyRule :: [Rule] -> [(Target, Rule)]
buildDependencyRule = fmap (\ rule -> (target rule, rule))

topologicalSort :: DependencyGraph -> Either Cycle [Target]
topologicalSort graph = let initialState = VisitState {
    colors   = Map.map (const White) (adjacencyGraph graph),
    ordering = [],
    path     = []
  } in case runStateT (mapM_ (visitNode graph) (Map.keys (adjacencyGraph graph))) initialState of
        Left cycle -> Left cycle
        Right (_, finalState) -> Right (reverse (ordering finalState))
-- runStateT action s0 :: Either Cycle ((), VisitState)

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

buildCyclePath :: Target -> [Target] -> [Target]
buildCyclePath target path = reverse (takeWhile (/= target) path) ++ [target]

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





