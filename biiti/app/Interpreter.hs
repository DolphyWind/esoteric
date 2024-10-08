module Interpreter where

import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Except as ET
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Parser
import Variable

type Stack = [Variable]

data Interpreter = Interpreter {
        getCommands :: [Command],
        getStack :: Stack,
        getIp :: Int,
        getAcc :: Variable,
        getCmp :: CompareState
}

type InterpreterState = ST.StateT Interpreter IO
type ExceptState = ET.ExceptT String InterpreterState

popStack :: ExceptState Variable
popStack = do
    interpreter <- lift ST.get
    let stck = getStack interpreter
    case stck of
        [] -> ET.throwE "Stack is empty!"
        (x:xs) -> do
            lift $ ST.put interpreter { getStack = xs }
            return x

pushStack :: Variable -> ExceptState ()
pushStack v = do
    interpreter <- lift ST.get
    let stck = getStack interpreter

    lift $ ST.put interpreter { getStack = v : stck }

runCommand :: Command -> ExceptState ()
runCommand (Push x) = do
    pushStack x

runCommand Pop = do
    _ <- popStack
    return ()

runCommand Add = operationHelper addVariable
runCommand Sub = operationHelper subtractVariable
runCommand Mul = operationHelper multiplyVariable
runCommand Div = operationHelper divideVariable

runCommand Cmp = do
    v1 <- popStack
    v2 <- popStack

    interpreter <- lift ST.get
    case compareVariable v2 v1 of
        Left err -> ET.throwE err
        Right c -> lift $ ST.put interpreter {getCmp = c}

runCommand (Jmp n) = do
    interpreter <- lift ST.get
    lift $ ST.put interpreter {getIp = n - 1}

runCommand Je = cmpHelper [Equal]
runCommand Jne = cmpHelper [Less, Greater]
runCommand Jg = cmpHelper [Greater]
runCommand Jge = cmpHelper [Greater, Equal]
runCommand Jl = cmpHelper [Less]
runCommand Jle = cmpHelper [Less, Equal]

runCommand Mov = do
    interpreter <- lift ST.get
    let stck = getStack interpreter
    case stck of
        [] -> ET.throwE "Empty Stack"
        (x:xs) -> lift $ ST.put interpreter { getAcc = x, getStack = xs }

runCommand Rtr = do
    interpreter <- lift ST.get
    let acc = getAcc interpreter
    pushStack acc

runCommand Print = do
    v <- popStack
    liftIO $ case v of
        Str s -> putStr s
        Int' i -> putStr $ show i
        Flt f -> putStr $ show f

runCommand Dup = do
    v <- popStack
    pushStack v
    pushStack v

runCommand Rot = do
    a <- popStack
    b <- popStack
    c <- popStack

    pushStack a
    pushStack c
    pushStack b

runCommand Index = do
    stringVar <- popStack
    case stringVar of
        (Str s) -> do
            index <- popStack
            case index of
                (Int' i) -> do
                    if i >= length s then
                        ET.throwE "Index out of bounds!"
                    else do
                        pushStack $ Str [s !! i]
                _ -> ET.throwE "Index should be an integer!"
        _ -> ET.throwE "Only strings are indexable!"

runCommand Read = do
    var <- liftIO $ do
        runParser parseVariableForRead <$> getLine

    case var of
        Left err -> ET.throwE err
        Right (_,v) -> pushStack v

    return ()

runCommand Nop = do
    return ()

operationHelper :: (Variable -> Variable -> Either String Variable) -> ExceptState ()
operationHelper f = do
    v1 <- popStack
    v2 <- popStack

    case f v2 v1 of
        Left err -> ET.throwE err
        Right v -> do
            pushStack v

cmpHelper :: [CompareState] -> ExceptState ()
cmpHelper xs = do
    interpreter <- lift ST.get
    let cmp = getCmp interpreter
    let ip = getIp interpreter
    when (cmp `elem` xs) $ lift $ ST.put interpreter { getIp = ip + 1 }
