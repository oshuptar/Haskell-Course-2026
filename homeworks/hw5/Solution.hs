module Solution(main) where
import Control.Monad.State
import Data.Map

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

-- 2:
data Expr = Num Int | Var String | Add Expr Expr | Mul Expr Expr | Neg Expr | Assign String Expr | Seq Expr Expr
eval :: Expr -> State (Map String Int) Int
eval (Num !val) = return val
eval (Var variable) = do
    map <- get
    case Data.Map.lookup variable map of
        Just val -> return val
eval (Add !exprL !exprR) = do
    resultL <- eval exprL
    resultR <- eval exprR
    return (resultL + resultR)
eval (Mul !exprL !exprR) = do
    resultL <- eval exprL
    resultR <- eval exprR
    return (resultL * resultR)
eval (Neg !expr) = do
    result <- eval expr
    return (-result)
eval (Assign var expr) = do
    result <- eval expr
    modify (Data.Map.insert var result)
    return result
eval (Seq exprL exprR) = do
    eval exprL
    eval exprR

runEval :: Expr -> Int
runEval expression = let (res,state) = runState (eval expression) Data.Map.empty in res


main :: IO ()
main = do
    putStrLn "Part1"
    let testProgram = [PUSH 3, PUSH 4, ADD, DUP, PUSH 2, MUL, SWAP, NEG, POP]
    let expected = [14]
    print $ runProg testProgram
    print $ runProg testProgram == expected

    let skipProgram = [POP, DUP, SWAP, ADD, MUL, NEG, PUSH 10, SWAP, ADD, MUL, PUSH 5, SWAP]
    let expected = [10,5]
    print $ runProg skipProgram
    print $ runProg skipProgram == expected

    putStrLn "\nPart2"
    let testEval = Seq (Assign "x" (Num 10)) (Seq (Assign "y" (Add (Var "x") (Num 5))) (Seq (Assign "z" (Mul (Var "y") (Neg (Num 2)))) (Add (Var "z") (Var "x"))))
    let expected = -20
    print $ runEval testEval
    print $ runEval testEval == expected

    let testEval = Seq (Assign "a" (Num 4)) (Seq (Assign "b" (Mul (Var "a") (Num 3))) (Seq (Assign "a" (Add (Var "b") (Neg (Var "a")))) (Mul (Var "a") (Add (Var "b") (Num 1)))))
    let expected = 104
    print $ runEval testEval
    print $ runEval testEval == expected

