module BuildSysLang.Parser where
import Control.Applicative
import BuildSysLang.AST (BuildFile (..), Rule(..), Target, Command(..))
import Data.Char (isDigit, isLetter, isSpace)

data PState = PState { psInput :: String, psLine :: Int, psCol :: Int } deriving (Show , Eq)
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

-- Runs a parser
parse :: Parser a -> String -> [(a, PState)]
parse p input = runParser p (PState input 1 1)

-- Parses many rules
parseBuildFile :: Parser BuildFile
parseBuildFile = do
    parseSpaces
    rules <- parseMany parseRule
    return (BuildFile rules)

-- Parses a generic rule
parseRule :: Parser Rule
parseRule = do
    _ <- parseSymbol "target"
    target <- parseTarget
    _ <- parseSymbol ":"
    deps <- parseDependencies
    _ <- parseSymbol "{"
    recipe <- parseRecipe
    _ <- parseSymbol "}"
    return (Rule target deps recipe)

-- Parser a generic target
parseTarget :: Parser Target
parseTarget = do
  c  <- parseLetter
  cs <- parseMany (parseLetter <|> parseDigit <|> parseChar '.' <|> parseChar '_')
  _ <- parseSpaces
  return (c : cs)

-- Parses dependency section
parseDependencies :: Parser [Target]
parseDependencies = undefined

-- Parses recipe section
parseRecipe :: Parser [Command]
parseRecipe = undefined

-- Parses a single command
parseCommand :: Parser Command
parseCommand = undefined

parseShell :: Parser Command
parseShell = undefined

parseEcho :: Parser Command
parseEcho = undefined

parseTouch :: Parser Command
parseTouch = undefined

-- Consumes one character
parseItem :: Parser Char
parseItem = Parser $ \parserState -> case psInput parserState of
    [] -> [] -- nothing to read
    (char:chars) -> [(char, advance char parserState {psInput = chars})]

-- Updates line/col based on what character was consumed
advance :: Char -> PState -> PState
advance '\n' state = state { psLine = psLine state + 1 , psCol = 1}
advance _ state = state { psCol = psCol state + 1 }

-- Parses a character matching the predicate
parseMatching :: (Char -> Bool) -> Parser Char
parseMatching predicate = do
    char <- parseItem
    if predicate char
        then return char
        else empty

-- Reading zero or more occurences of an element:
parseMany :: Parser a -> Parser [a]
parseMany parser = parseSome parser <|> return []

-- Reading one or more occurences of an element:
parseSome :: Parser a -> Parser [a]
parseSome parser = do
    item <- parser
    rest <- parseMany parser
    return (item:rest)

-- Parses a space character
parseSpace :: Parser Char
parseSpace = parseMatching isSpace

parseSpaces :: Parser ()
parseSpaces = do { _ <- parseMany parseSpace; return () }

parseToken :: Parser a -> Parser a
parseToken parser = do { value <- parser; parseSpaces; return value }

parseSymbol :: String -> Parser String
parseSymbol cs = parseToken (parseString cs)

-- Parses a string given as an input
parseString :: String -> Parser String
parseString "" = return []
parseString (c:cs) = do
    _ <- parseMatching (==c)
    _ <- parseString cs
    return (c:cs)

-- Parses a digit
parseDigit :: Parser Char
parseDigit = parseMatching isDigit

-- Parses a letter
parseLetter :: Parser Char
parseLetter = parseMatching isLetter

-- Parses a specific character
parseChar :: Char -> Parser Char
parseChar char = parseMatching (== char)

