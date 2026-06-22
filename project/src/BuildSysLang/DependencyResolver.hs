module BuildSysLang.DependencyResolver where
import qualified Data.Map as Map
import Data.Map (Map)
import BuildSysLang.AST (Target, Rule (..), BuildFile (..), Command (..))

data DependencyGraph = DependencyGraph {
    adjacencyGraph :: Map Target [Target],
    dependencyRule :: Map Target Rule
} deriving (Show, Eq)

data Color = White | Gray | Black deriving (Show, Eq)

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


