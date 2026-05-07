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

--3:
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM left right i 0 = do
    cache <- get
    modify (Data.Map.insert (i, 0) i)
    return i
editDistM left right 0 j = do
    cache <- get
    modify (Data.Map.insert (0, j) j)
    return j
editDistM xs ys i j = do
    cache <- get
    case Data.Map.lookup (i, j) cache of
        Just dist -> return dist
        Nothing ->
            if xs !! (i-1) == ys !! (j-1)
                then do
                    result <- editDistM xs ys (i-1) (j-1)
                    modify (Data.Map.insert (i, j) result)
                    return result
                else do
                    del <- editDistM xs ys (i-1) j
                    ins <- editDistM xs ys i (j-1)
                    sub <- editDistM xs ys (i-1) (j-1)
                    let result = 1 + minimum [del, ins, sub]
                    modify (Data.Map.insert (i, j) result)
                    return result

editDistance :: String -> String -> Int
editDistance left right = let (res, state) =
                                runState (editDistM left right (length left) (length right)) Data.Map.empty
                            in res

testEditDistance :: String -> String -> Int -> (Bool, Int)
testEditDistance left right expected = let result = editDistance left right in (result == expected, result)

main :: IO ()
main = do
    putStrLn "Task1"
    let testProgram = [PUSH 3, PUSH 4, ADD, DUP, PUSH 2, MUL, SWAP, NEG, POP]
    let expected = [14]
    print $ runProg testProgram
    print $ runProg testProgram == expected

    let skipProgram = [POP, DUP, SWAP, ADD, MUL, NEG, PUSH 10, SWAP, ADD, MUL, PUSH 5, SWAP]
    let expected = [10,5]
    print $ runProg skipProgram
    print $ runProg skipProgram == expected

    putStrLn "\nTask2"
    let testEval = Seq (Assign "x" (Num 10)) (Seq (Assign "y" (Add (Var "x") (Num 5))) (Seq (Assign "z" (Mul (Var "y") (Neg (Num 2)))) (Add (Var "z") (Var "x"))))
    let expected = -20
    print $ runEval testEval
    print $ runEval testEval == expected

    let testEval = Seq (Assign "a" (Num 4)) (Seq (Assign "b" (Mul (Var "a") (Num 3))) (Seq (Assign "a" (Add (Var "b") (Neg (Var "a")))) (Mul (Var "a") (Add (Var "b") (Num 1)))))
    let expected = 104
    print $ runEval testEval
    print $ runEval testEval == expected

    putStrLn "\nTask3"
    print $ testEditDistance "" "" 0
    print $ testEditDistance "" "abc" 3
    print $ testEditDistance "cat" "cut" 1
    print $ testEditDistance "cat" "cart" 1
    print $ testEditDistance "haskell" "haskell" 0
    print $ testEditDistance "kitten" "sitting" 3
    print $ testEditDistance "flaw" "lawn" 2
    print $ testEditDistance "intention" "execution" 5
