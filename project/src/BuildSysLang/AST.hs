module BuildSysLang.AST (BuildFile, Rule, Command) where

type Target = String
newtype BuildFile = BuildFile [Rule] deriving (Show, Eq)

data Rule = Rule {
    target :: Target,
    dependencies :: [Target],
    recipe :: Command
} deriving (Show, Eq)

data Command = Shell String | Echo String | Touch Target deriving (Show, Eq)