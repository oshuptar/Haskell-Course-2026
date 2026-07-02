module BuildSysLang.BuildRunner (runBuild) where
import BuildSysLang.Parser ( parseProgram )    
import BuildSysLang.DependencyResolver (buildDependencyGraph, build, DependencyGraph (adjacencyGraph))    
import BuildSysLang.FreshnessRule (loadHashStore, mkFreshnessCheck, constructHashStore, persistHashStore)
import Control.Monad (when)
import Control.Monad.State (runStateT)
import qualified Data.Map as Map

runBuild :: FilePath -> FilePath -> IO ()
runBuild buildFilePath hashStorePath = do
    source <- readFile buildFilePath
    case parseProgram source of
        Left err -> putStrLn $ ("Parse Error: " ++ err)
        Right buildFile -> do
            let graph = buildDependencyGraph buildFile
            oldStore <- loadHashStore hashStorePath
            result <- build graph (mkFreshnessCheck graph oldStore)
            when result $ do
                (_, newStore) <- runStateT (constructHashStore graph (Map.keys (adjacencyGraph graph))) Map.empty
                persistHashStore hashStorePath newStore
