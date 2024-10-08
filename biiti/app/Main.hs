{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.IO
import Parser
import Variable
import Interpreter
import qualified Control.Monad.Trans.Except as ET
import qualified Control.Monad.Trans.State as ST
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as OA
import Options.Applicative
import qualified Transpiler as T
import Transpiler (serializeNewCommand)

data Options = Options {
    transpile :: Bool,
    filename :: String,
    mode :: String
} deriving(Show, Eq)

optionsParser :: OA.Parser Options
optionsParser = do
    transpile <- OA.switch
      (  long "transpile"
      <> short 't'
      <> help "Transpile code to specified mode instead of running it")
    filename <- strOption
      (  long "filename"
      <> short 'f'
      <> help "File to run"
      <> metavar "FILENAME")
    mode <- strOption
      (  long "mode"
      <> short 'm'
      <> help "Mode, either \"sane\" or \"biiti\". In sane mode, everything works *as expected*."
      <> value "biiti"
      <> showDefault)
    return Options{..}

optsParserInfo :: ParserInfo Options
optsParserInfo = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "BIITI is a stack-based esoteric programming language designed by DolphyWind."
  <> header "BIITI - Beauty Is In The Implementation" )

checkErrorHelper :: [(Int, Either String Command)] -> [Either String (Int, Command)]
checkErrorHelper [] = []
checkErrorHelper ((l, Left err):rest) = Left ("Error on line " ++ show l ++ ": " ++ err) : checkErrorHelper rest
checkErrorHelper ((l, Right cmd):rest) = Right (l, cmd) : checkErrorHelper rest

checkErrors :: [(Int, Either String Command)] -> Either String [(Int, Command)]
checkErrors xs = sequenceA (checkErrorHelper xs)

removeLines :: [(Int, Command)] -> [Command]
removeLines [] = []
removeLines ((_, c):rest) = c:removeLines rest

updateInterpreter :: Interpreter -> Interpreter
updateInterpreter (Interpreter cmds stck ip acc cmp) = Interpreter cmds stck (ip + 1) acc cmp

makeInterpreter :: [Command] -> Interpreter
makeInterpreter cmds = Interpreter cmds [] 1 (Int' 0) NoCmp

runProgram :: Interpreter -> IO ()
runProgram interpreter = do
    let ip = getIp interpreter
    let cmds = getCommands interpreter
    let len = length cmds

    if ip > len then
        return ()
        -- print $ getStack interpreter
    else do
        let cmd = cmds !! (ip - 1)
        (result, nextState) <- ST.runStateT (ET.runExceptT (runCommand cmd)) interpreter

        case result of
            Left err -> liftIO $ hPutStrLn stderr ("Error on line " ++ show ip ++ ": " ++ err)
            Right () -> do
                let updatedState = updateInterpreter nextState
                runProgram updatedState

parseSaneMode :: String -> Either String [Command]
parseSaneMode source = do
    let commandList = unwrapCommand . sequenceA . runParser parseCommand <$> lines source
    let commandsWithLines = zip [1..length commandList] commandList
    case checkErrors commandsWithLines of
        Left err -> Left err
        Right commands -> Right $ removeLines commands

parseBiitiMode :: String -> Either String [Command]
parseBiitiMode source = do
    let commandList = unwrapCommand . sequenceA . runParser (T.invTranspileSingle <$> T.parseCommand') <$> lines source
    let commandsWithLines = zip [1..length commandList] commandList
    case checkErrors commandsWithLines of
        Left err -> Left err
        Right commands -> Right $ removeLines commands

runHelper :: (String -> Either String [Command]) -> String -> IO ()
runHelper parser source = do
    let commands = parser source
    case commands of
        Left err -> hPutStrLn stderr err
        Right cmds -> runProgram $ makeInterpreter cmds

unlinesFixed :: [String] -> String
unlinesFixed = init . unlines

main :: IO ()
main = do
    opts <- execParser optsParserInfo
    let file_name = filename opts
    content <- readFile file_name

    if transpile opts then do
        if mode opts == "sane" then do
            let commands = parseSaneMode content
            case commands of
                Left err -> hPutStrLn stderr err
                Right cmds -> putStrLn $ unlinesFixed $ serializeNewCommand <$> T.transpile cmds
        else if mode opts == "biiti" then do
            let commands = parseBiitiMode content
            case commands of
                Left err -> hPutStrLn stderr err
                Right cmds -> putStrLn $ unlinesFixed $ serializeCommand <$> cmds
        else do
            hPutStrLn stderr "Invalid mode! Avalible modes are \"sane\" and \"biiti\""
    else do
        if mode opts == "sane" then do
            runHelper parseSaneMode content
        else if mode opts == "biiti" then do
            runHelper parseBiitiMode content
        else do
            hPutStrLn stderr "Invalid mode! Avalible modes are \"sane\" and \"biiti\""

