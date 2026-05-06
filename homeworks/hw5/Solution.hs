module Solution(main) where
import Control.Monad.State

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG

execInstr :: Instr -> State [Int] ()
execInstr (PUSH val) = do modify (val :)
execInstr POP = do
    stack <- get
    case stack of
        [] -> return ()
        x:xs -> put xs
execInstr DUP = do
    stack <- get
    case stack of
        [] -> return ()
        x:xs -> modify (x:)
execInstr SWAP = do
    stack <- get
    case stack of
        x1:x2:xs -> put (x2:x1:xs)
        _ -> return ()
execInstr ADD = do
    stack <- get
    case stack of
        x1:x2:xs -> put ((x1 + x2) : xs)
        _ -> return ()
execInstr MUL = do
    stack <- get
    case stack of
        x1:x2:xs -> put ((x1 * x2) : xs)
        _ -> return ()
execInstr NEG = do
    stack <- get
    case stack of
        []   -> return ()
        x:xs -> put ((-x) : xs)

execProg :: [Instr] -> State [Int] ()
execProg [] = return ()
execProg (head:remaining) = do
    execInstr head
    execProg remaining

runProg :: [Instr] -> [Int]
runProg instructions = let (res, state) = runState (execProg instructions) [] in state


main :: IO ()
main = do
    let testProgram = [PUSH 3, PUSH 4, ADD, DUP, PUSH 2, MUL, SWAP, NEG, POP]
    let expected = [14]
    print $ runProg testProgram
    print $ runProg testProgram == expected

    let skipProgram = [POP, DUP, SWAP, ADD, MUL, NEG, PUSH 10, SWAP, ADD, MUL, PUSH 5, SWAP]
    let skipExpected = [10,5]
    print $ runProg skipProgram
    print $ runProg skipProgram == skipExpected

