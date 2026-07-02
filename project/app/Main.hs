module Main (main) where

import BuildSysLang.BuildRunner (runBuild)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [buildFilePath] -> runBuild buildFilePath ".hashes.json"
        [buildFilePath, hashStorePath] -> runBuild buildFilePath hashStorePath
        _ -> putStrLn "Usage: buildsys <buildfile> [hashstore]"