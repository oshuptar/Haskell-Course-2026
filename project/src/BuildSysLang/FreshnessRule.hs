{- HLINT ignore "Redundant lambda" -}
module BuildSysLang.FreshnessRule (mkFreshnessCheck, loadHashStore, persistHashStore, constructHashStore, HashStore, Hash) where
import BuildSysLang.AST (Target, Rule(..))
import BuildSysLang.DependencyResolver (FreshnessCheck, DependencyGraph(..))
import Data.Hashable (hash)
import Data.Aeson (encodeFile, decodeFileStrict)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import System.Directory (doesFileExist)
import Control.Monad.State (StateT, modify, lift, execStateT)
import Data.Map (Map)

type Hash = Int
type HashStore = Map Target Hash

mkFreshnessCheck :: DependencyGraph -> HashStore -> FreshnessCheck
mkFreshnessCheck graph oldHashes = \target -> do
    newHash <- hashTarget graph target
    return (Map.lookup target oldHashes /= Just newHash)

hashTarget :: DependencyGraph -> Target -> IO Hash
hashTarget graph target = case Map.lookup target (dependencyRule graph) of
    Just rule -> hashRule rule
    Nothing -> hashFileContent target -- pure content

hashRule :: Rule -> IO Hash
hashRule rule = do
    outputHash <- hashFileContent (target rule)
    return (hash (show (dependencies rule, recipe rule), outputHash))

hashFileContent :: Target -> IO Hash
hashFileContent path = do
    exists <- doesFileExist path
    if exists then hash <$> BS.readFile path
              else return 0

loadHashStore :: String -> IO HashStore
loadHashStore path = do
    exists <- doesFileExist path
    if exists then do
        result <- decodeFileStrict path
        case result of
            Just store -> return store
            Nothing -> return Map.empty
    else return Map.empty

persistHashStore :: String -> HashStore -> IO ()
persistHashStore = encodeFile
 
constructHashStore :: [Target] -> DependencyGraph -> StateT HashStore IO ()
constructHashStore [] graph = return ()
constructHashStore (target:rest) graph = do
    newHash <- lift (hashTarget graph target)
    modify (Map.insert target newHash)
    constructHashStore rest graph
