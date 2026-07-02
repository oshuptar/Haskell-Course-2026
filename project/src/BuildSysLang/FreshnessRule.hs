{- HLINT ignore "Redundant lambda" -}
module BuildSysLang.FreshnessRule (mkFreshnessCheck, loadHashStore, persistHashStore, constructHashStore, HashStore, Hash) where
import BuildSysLang.AST (Target, Rule(..))
import BuildSysLang.DependencyResolver (FreshnessCheck, DependencyGraph(..))
import Data.Hashable (hash)
import Data.Aeson (encodeFile, decodeFileStrict)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import System.Directory (doesFileExist)
import Control.Monad.State (StateT, modify, lift)
import Data.Map (Map)

type Hash = Int
type HashStore = Map Target Hash

mkFreshnessCheck :: DependencyGraph -> HashStore -> FreshnessCheck
mkFreshnessCheck graph oldHashes = \trgt -> do
    newHash <- hashTarget graph trgt
    return (Map.lookup trgt oldHashes /= Just newHash)

hashTarget :: DependencyGraph -> Target -> IO Hash
hashTarget graph trgt = case Map.lookup trgt (dependencyRule graph) of
    Just rule -> hashRule rule
    Nothing -> hashFileContent trgt -- pure content

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
 
constructHashStore :: DependencyGraph -> [Target] -> StateT HashStore IO ()
constructHashStore _ [] = return ()
constructHashStore graph (trgt:rest) = do
    newHash <- lift (hashTarget graph trgt)
    modify (Map.insert trgt newHash)
    constructHashStore graph rest
