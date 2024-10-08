module Transpiler where

import Variable
import Parser
import Control.Applicative
import Data.Char

data NewCommand = Mov' Variable
    | Jne'
    | Jl'
    | Jg'
    | Jle'
    | Cmp'
    | Mul'
    | Sub' Int
    | Jge'
    | Print'
    | Rtr'
    | Pop'
    | Dup'
    | Push'
    | Jmp'
    | Div'
    | Je'
    | Add'
    | Index'
    | Rot'
    | Read'
    | Nop'
    deriving (Show, Eq)

serializeNewCommand :: NewCommand -> String
serializeNewCommand (Mov' (Int' i)) = "MOV " ++ show i
serializeNewCommand (Mov' (Flt f)) = "MOV " ++ show f
serializeNewCommand (Mov' (Str s)) = "MOV " ++ show s
serializeNewCommand Jne' = "JNE"
serializeNewCommand Jl' = "JL"
serializeNewCommand Jg' = "JG"
serializeNewCommand Jle' = "JLE"
serializeNewCommand Cmp' = "CMP"
serializeNewCommand Mul' = "MUL"
serializeNewCommand (Sub' i) = "SUB " ++ show i
serializeNewCommand Jge' = "JGE"
serializeNewCommand Print' = "PRINT"
serializeNewCommand Rtr' = "RTR"
serializeNewCommand Pop' = "POP"
serializeNewCommand Dup' = "DUP"
serializeNewCommand Push' = "PUSH"
serializeNewCommand Jmp' = "JMP"
serializeNewCommand Div' = "DIV"
serializeNewCommand Je' = "JE"
serializeNewCommand Add' = "ADD"
serializeNewCommand Index' = "INDEX"
serializeNewCommand Rot' = "ROT"
serializeNewCommand Read' = "READ"
serializeNewCommand Nop' = ""

transpile :: [Command] -> [NewCommand]
transpile = map transpileSingle

transpileSingle :: Command -> NewCommand
transpileSingle (Push v) = Mov' v
transpileSingle Pop = Jne'
transpileSingle Add = Jl'
transpileSingle Sub = Jg'
transpileSingle Mul = Jle'
transpileSingle Div = Cmp'
transpileSingle Cmp = Mul'
transpileSingle (Jmp l) = Sub' l
transpileSingle Je = Jge'
transpileSingle Jne = Print'
transpileSingle Jg = Rtr'
transpileSingle Jge = Pop'
transpileSingle Jl = Dup'
transpileSingle Jle = Push'
transpileSingle Mov = Jmp'
transpileSingle Rtr = Div'
transpileSingle Print = Je'
transpileSingle Dup = Add'
transpileSingle Rot = Index'
transpileSingle Index = Read'
transpileSingle Read = Rot'
transpileSingle Nop = Nop'

invTranspile :: [NewCommand] -> [Command]
invTranspile = map invTranspileSingle

invTranspileSingle :: NewCommand -> Command
invTranspileSingle (Mov' v) = Push v
invTranspileSingle Jne' = Pop
invTranspileSingle Jl' = Add
invTranspileSingle Jg' = Sub
invTranspileSingle Jle' = Mul
invTranspileSingle Cmp' = Div
invTranspileSingle Mul' = Cmp
invTranspileSingle (Sub' l) = Jmp l
invTranspileSingle Jge' = Je
invTranspileSingle Print' = Jne
invTranspileSingle Rtr' = Jg
invTranspileSingle Pop' = Jge
invTranspileSingle Dup' = Jl
invTranspileSingle Push' = Jle
invTranspileSingle Jmp' = Mov
invTranspileSingle Div' = Rtr
invTranspileSingle Je' = Print
invTranspileSingle Add' = Dup
invTranspileSingle Rot' = Read
invTranspileSingle Index' = Rot
invTranspileSingle Read' = Index
invTranspileSingle Nop' = Nop

helperParser' :: NewCommand -> Parser NewCommand
helperParser' c = Parser $ \s -> return (s, c)

parseMov' :: Parser NewCommand
parseMov' = Mov' <$> (parseWord "MOV" *> (parseChar ' ' <|> parseChar '\t' <|> errorParser "At least one whitespace character is required after the \"MOV\" keyword!") *> parseWs *>
            ((Str <$> parseString) <|> (Flt <$> parseFloat) <|> (Int' <$> parseInteger)))

parseJne' :: Parser NewCommand
parseJne' = parseWord "JNE" *> helperParser' Jne'

parseJl' :: Parser NewCommand
parseJl' = parseWord "JL" *> helperParser' Jl'

parseJg' :: Parser NewCommand
parseJg' = parseWord "JG" *> helperParser' Jg'

parseJle' :: Parser NewCommand
parseJle' = parseWord "JLE" *> helperParser' Jle'

parseCmp' :: Parser NewCommand
parseCmp' = parseWord "CMP" *> helperParser' Cmp'

parseMul' :: Parser NewCommand
parseMul' = parseWord "MUL" *> helperParser' Mul'

parseSub' :: Parser NewCommand
parseSub' = Sub' <$> (parseWord "SUB" *>
                   (parseChar ' ' <|> parseChar '\t' <|> errorParser "At least one whitespace character is required after the \"SUB\" keyword!") *> parseWs *>
                   parseInteger)

parseJge' :: Parser NewCommand
parseJge' = parseWord "JGE" *> helperParser' Jge'

parsePrint' :: Parser NewCommand
parsePrint' = parseWord "PRINT" *> helperParser' Print'

parseRtr' :: Parser NewCommand
parseRtr' = parseWord "RTR" *> helperParser' Rtr'

parsePop' :: Parser NewCommand
parsePop' = parseWord "POP" *> helperParser' Pop'

parseDup' :: Parser NewCommand
parseDup' = parseWord "DUP" *> helperParser' Dup'

parsePush' :: Parser NewCommand
parsePush' = parseWord "PUSH" *> helperParser' Push'

parseJmp' :: Parser NewCommand
parseJmp' = parseWord "JMP" *> helperParser' Jmp'

parseDiv' :: Parser NewCommand
parseDiv' = parseWord "DIV" *> helperParser' Div'

parseJe' :: Parser NewCommand
parseJe' = parseWord "JE" *> helperParser' Je'

parseAdd' :: Parser NewCommand
parseAdd' = parseWord "ADD" *> helperParser' Add'

parseRot' :: Parser NewCommand
parseRot' = parseWord "ROT" *> helperParser' Rot'

parseIndex' :: Parser NewCommand
parseIndex' = parseWord "INDEX" *> helperParser' Index'

parseRead' :: Parser NewCommand
parseRead' = parseWord "READ" *> helperParser' Read'

nopIfEmptyParser' :: Parser NewCommand
nopIfEmptyParser' = Parser $ \s -> if null (dropWhile isSpace s) then return (s, Nop') else Left ""

parseCommand' :: Parser NewCommand
parseCommand' = parseWs *> (parsePush' <|> parsePop' <|> parseAdd' <|> parseSub' <|> parseMul' <|> parseDiv' <|> parseCmp' <|>
                parseJmp' <|> parseJe' <|> parseJne' <|> parseJge' <|> parseJg' <|> parseJle' <|> parseJl' <|> parseMov' <|>
                parseRtr' <|> parsePrint' <|> parseDup' <|> parseRot' <|> parseIndex' <|> parseRead' <|> nopIfEmptyParser' <|> errorAppendingParser "Not a word: ") <* parseWs

