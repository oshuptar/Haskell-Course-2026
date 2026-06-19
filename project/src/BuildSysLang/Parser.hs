module BuildSysLang.Parser () where 
import Control.Applicative
import BuildSysLang.AST (BuildFile, Rule, Target, Command)
import Control.Concurrent (Chan)

data PState = PState { psInput :: String, psLine :: Int, psCol :: Int }
newtype Parser a = Parser { runParser :: PState -> [(a, PState)] }

-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
-- (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b

-- Remember that Monad is a narrower concept than Applicative Functor > Applicative > Monad.
instance Monad Parser where
    -- (>>=)  :: m a -> (a -> m b) -> m b 
    -- return ::   a -> m a 
    return = pure
    (Parser parser) >>= g = Parser $ \parserState -> 
        [(parsed', leftover') | (parsed, leftover) <- parser parserState,
                                (parsed', leftover') <- runParser (g parsed) leftover  ]
        


instance Applicative Parser where
    -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pure parsed = Parser $ \parserState -> [(parsed, parserState)]
    pf <*> px = pf >>= \f -> px >>= \x -> return (f x)



instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f p = p >>= return . f


-- Sequencing fundamentally cannot say "or." You need a separate operation: "try the left parser; if it fails, try the right." That operation is <|>, and the typeclass that provides it is Alternative.
instance Alternative Parser where
    -- empty :: f a
    empty = Parser $ \_ -> []
    -- <|> :: f a -> f a -> f a
    (Parser parser) <|> (Parser fallback) = Parser $ \parserState -> 
        case parser parserState of
            [] -> fallback parserState
            results -> results

-- Parses many rules
parseBuildFile :: String -> Parser BuildFile
parseBuildFile = undefined

-- IO operation that reads a file and transforms into a string. A function that receives a file name and returns an IO computation that produces a String
readFile :: FilePath -> IO String
readFile = undefined

-- Parses a generic rule
parseRule :: String -> Parser Rule
parseRule = undefined

-- Parser a generic target
parseTarget :: String -> Parser Target
parseTarget = undefined

-- Parses dependency section
parseDependencies :: String -> Parser [Target]
parseDependencies = undefined
114 x 16
1 Hidden Terminal
use stack to build

stack build


-- Parses recipe section
parseRecipe :: String -> [Command]
parseRecipe = undefined

-- Parses a single command
parseCommand :: String -> Command
parseCommand = undefined

-- Consumes one character
parseItem :: Parser Char
parseItem = Parser $ \parserState -> case psInput parserState of
    [] -> [] -- nothing to read
    (char:chars) -> [(char, parserState {psInput = chars})]

-- Updates line/col based on what character was consumed
advance :: Char -> PState -> PState
advance = undefined

-- Parses a character matching the predicate
parseMatching :: (Char -> Bool) -> Parser Char
parseMatching = undefined

-- Reading zero or more occurences of an element:
parseMany :: Parser a -> Parser [a]
parseMany = undefined

-- Reading one or more occurences of an element:
parseSome :: Parser a -> Parser [a]
parseSome = undefined

-- Parses a space character
parseSpace :: Parser Char
parseSpace = undefined

-- Parses a digit
parseDigit :: Parser Char
parseDigit = undefined

-- Parses a letter
parseLetter :: Parser Char
parseLetter = undefined

-- Parses a specific character
parseChar :: Char -> Parser Char
parseChar = undefined

