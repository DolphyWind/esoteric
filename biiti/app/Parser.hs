module Parser where

import Control.Applicative
import Variable
import Data.List
import Data.Char

data Command = Push Variable
             | Pop
             | Add
             | Sub
             | Mul
             | Div
             | Cmp
             | Jmp Int
             | Je
             | Jne
             | Jg
             | Jge
             | Jl
             | Jle
             | Mov
             | Rtr
             | Print
             | Dup
             | Rot
             | Index
             | Read
             | Nop
    deriving (Show,Eq)

newtype Parser a = Parser { runParser :: String -> Either String (String, a) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser pa) = Parser $ \s -> do
        (rest, a) <- pa s
        return (rest, f a)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s -> Right (s, x)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pf) <*> (Parser pa) = Parser $ \s -> do
        (rest, f) <- pf s
        (rest', a) <- pa rest
        return (rest', f a)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> Left "Unknown error"

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser $ \s ->
        case p1 s of
            Left _ -> p2 s
            Right x -> Right x

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (Parser pa) >>= f = Parser $ \s -> do
        (rest, a) <- pa s
        runParser (f a) rest

serializeCommand :: Command -> String
serializeCommand (Push (Int' i)) = "PUSH " ++ show i
serializeCommand (Push (Flt f)) = "PUSH " ++ show f
serializeCommand (Push (Str s)) = "PUSH " ++ show s
serializeCommand Pop = "POP"
serializeCommand Add = "ADD"
serializeCommand Sub = "SUB"
serializeCommand Mul = "MUL"
serializeCommand Div = "DIV"
serializeCommand Cmp = "CMP"
serializeCommand (Jmp i) = "JMP " ++ show i
serializeCommand Je = "JE"
serializeCommand Jne = "JNE"
serializeCommand Jg = "JG"
serializeCommand Jge = "JGE"
serializeCommand Jl = "JL"
serializeCommand Jle = "JLE"
serializeCommand Mov = "MOV"
serializeCommand Rtr = "RTR"
serializeCommand Print = "PRINT"
serializeCommand Dup = "DUP"
serializeCommand Rot = "ROT"
serializeCommand Index = "INDEX"
serializeCommand Read = "READ"
serializeCommand Nop = ""

stringToInt :: String -> Int
stringToInt = read

parseNull :: Parser Char
parseNull = Parser $ \s -> return (s, '\0')

parseChar :: Char -> Parser Char
parseChar c = Parser f
    where f [] = Left ("Empty line. Expected '" ++ [c] ++ "'")
          f (x:xs)
            | x == c = Right (xs, c)
            | otherwise = Left ("Incorrect character! Expected '" ++ [c] ++ "', Got '" ++ [x] ++ "'")

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser $ \s -> do
    let (str, rest) = span f s
    return (rest, str)

parseWs :: Parser String
parseWs = parseSpan (`elem` " \t")

parseWord :: String -> Parser String
parseWord str = Parser $ \s -> do
    if str `isPrefixOf` s then
        return (drop (length str) s, str)
    else Left $ "Cannot match word \"" ++ str ++ "\""

helperParser :: Command -> Parser Command
helperParser c = Parser $ \s -> return (s, c)

parseUnsignedInteger :: Parser Int
parseUnsignedInteger = Parser $ \s ->
    case span isNumber s of
        ("", _) -> Left $ "Not starting with an integer: " ++ s
        (digits, rest) -> return (rest, stringToInt digits)

parseInteger :: Parser Int
parseInteger = (f <$> (parseChar '+' <|> parseChar '-' <|> parseNull)) <*> parseUnsignedInteger
    where f :: Char -> (Int -> Int)
          f '-' = (*(-1))
          f _ = (*1)

parseFloat :: Parser Float
parseFloat = (\x _ z -> read (show x ++ "." ++ show z)) <$> parseInteger <*> parseChar '.' <*> parseUnsignedInteger

parseAnyChar :: Parser Char
parseAnyChar = Parser $ \s -> do
    if null s then
        Left "Empty Line"
    else Right (tail s, head s)

parseString :: Parser String
parseString = parseChar '"' *> parseStringContent <* parseChar '"'

parseStringContent :: Parser String
parseStringContent = Parser $ \s -> case s of
    ('"':_) -> Right (s, "")  -- Stop at the closing quote
    ('\\':x:xs) -> do
        let escapedChar = case x of
                '"'  -> '"'
                '\\' -> '\\'
                'n'  -> '\n'
                't'  -> '\t'
                -- Add more escape sequences as necessary
                _    -> x  -- Just return the character as is if it's not a special escape sequence
        (rest, str) <- runParser parseStringContent xs
        return (rest, escapedChar : str)
    (x:xs) -> do
        (rest, str) <- runParser parseStringContent xs
        return (rest, x : str)
    [] -> Left "Unterminated string"

parseVariable :: Parser Variable
parseVariable = (Str <$> parseString) <|> (Flt <$> parseFloat) <|> (Int' <$> parseInteger) <|> errorAppendingParser "Not a variable: "

parseVariableForRead :: Parser Variable
parseVariableForRead = (Flt <$> parseFloat) <|> (Int' <$> parseInteger) <|> (Str <$> Parser (\s -> Right ("", s) ))

parsePush :: Parser Command
parsePush = Push <$> (parseWord "PUSH" *> (parseChar ' ' <|> parseChar '\t' <|> errorParser "At least one whitespace character is required after the \"PUSH\" keyword!") *> parseWs *>
            parseVariable)

parsePop :: Parser Command
parsePop = parseWord "POP" *> helperParser Pop

parseAdd :: Parser Command
parseAdd = parseWord "ADD" *> helperParser Add

parseSub :: Parser Command
parseSub = parseWord "SUB" *> helperParser Sub

parseMul :: Parser Command
parseMul = parseWord "MUL" *> helperParser Mul

parseDiv :: Parser Command
parseDiv = parseWord "DIV" *> helperParser Div

parseCmp :: Parser Command
parseCmp = parseWord "CMP" *> helperParser Cmp

parseJmp :: Parser Command
parseJmp = Jmp <$> (parseWord "JMP" *>
                   (parseChar ' ' <|> parseChar '\t' <|> errorParser "At least one whitespace character is required after the \"JMP\" keyword!") *> parseWs *>
                   parseInteger)

parseJe :: Parser Command
parseJe = parseWord "JE" *> helperParser Je

parseJne :: Parser Command
parseJne = parseWord "JNE" *> helperParser Jne

parseJg :: Parser Command
parseJg = parseWord "JG" *> helperParser Jg

parseJge :: Parser Command
parseJge = parseWord "JGE" *> helperParser Jge

parseJl :: Parser Command
parseJl = parseWord "JL" *> helperParser Jl

parseJle :: Parser Command
parseJle = parseWord "JLE" *> helperParser Jle

parseMov :: Parser Command
parseMov = parseWord "MOV" *> helperParser Mov

parseRtr :: Parser Command
parseRtr = parseWord "RTR" *> helperParser Rtr

parsePrint :: Parser Command
parsePrint = parseWord "PRINT" *> helperParser Print

parseDup :: Parser Command
parseDup = parseWord "DUP" *> helperParser Dup

parseRot :: Parser Command
parseRot = parseWord "ROT" *> helperParser Rot

parseIndex :: Parser Command
parseIndex = parseWord "INDEX" *> helperParser Index

parseRead :: Parser Command
parseRead = parseWord "READ" *> helperParser Read

errorParser :: String -> Parser a
errorParser str = Parser $ \_ -> Left str

nopIfEmptyParser :: Parser Command
nopIfEmptyParser = Parser $ \s -> if null (dropWhile isSpace s) then return (s, Nop) else Left ""

errorAppendingParser :: String -> Parser a
errorAppendingParser str = Parser $ \s -> Left $ str ++ "\"" ++ s ++ "\""

parseCommand :: Parser Command
parseCommand = parseWs *> (parsePush <|> parsePop <|> parseAdd <|> parseSub <|> parseMul <|> parseDiv <|> parseCmp <|>
                parseJmp <|> parseJe <|> parseJne <|> parseJge <|> parseJg <|> parseJle <|> parseJl <|> parseMov <|>
                parseRtr <|> parsePrint <|> parseDup <|> parseRot <|> parseIndex <|> parseRead <|> nopIfEmptyParser <|> errorAppendingParser "Not a word: ") <* parseWs


unwrapCommand :: (String, Either String Command) -> Either String Command
unwrapCommand ("", Right cmd) = Right cmd
unwrapCommand (str, Right _) = Left $ "Unknown suffix: " ++ str
unwrapCommand (_, Left err) = Left err
