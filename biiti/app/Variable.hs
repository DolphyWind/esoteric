module Variable where

data Variable = Str { getString :: String }
              | Flt { getFloat :: Float }
              | Int' { getInt :: Int }
              deriving (Show, Eq)

data CompareState = Less
                  | Equal
                  | Greater
                  | NoCmp
              deriving (Show, Eq, Enum)

compare' :: Ord a => a -> a -> CompareState
compare' x y = case compare x y of
    LT -> Less
    EQ -> Equal
    GT -> Greater

typeName :: Variable -> String
typeName (Str _) = "string"
typeName (Flt _) = "float"
typeName (Int' _) = "integer"

operationError :: String -> Variable -> Variable -> String
operationError op v1 v2 = "Cannot apply operation \"" ++ op ++ "\" to types \"" ++ typeName v1 ++ "\" and \"" ++ typeName v2 ++ "\""

addVariable :: Variable -> Variable -> Either String Variable
addVariable (Str x) (Str y) = Right (Str (x ++ y))
addVariable (Flt x) (Flt y) = Right (Flt (x + y))
addVariable (Flt x) (Int' y) = Right (Flt (x + fromIntegral y))
addVariable (Int' x) (Flt y) = Right (Flt (fromIntegral x + y))
addVariable (Int' x) (Int' y) = Right (Int' (x + y))
addVariable v1 v2 = Left $ operationError "addition" v1 v2

subtractVariable :: Variable -> Variable -> Either String Variable
subtractVariable (Flt x) (Flt y) = Right (Flt (x - y))
subtractVariable (Flt x) (Int' y) = Right (Flt (x - fromIntegral y))
subtractVariable (Int' x) (Flt y) = Right (Flt (fromIntegral x - y))
subtractVariable (Int' x) (Int' y) = Right (Int' (x - y))
subtractVariable v1 v2 = Left $ operationError "subtraction" v1 v2

multiplyVariable :: Variable -> Variable -> Either String Variable
multiplyVariable (Flt x) (Flt y) = Right (Flt (x * y))
multiplyVariable (Flt x) (Int' y) = Right (Flt (x * fromIntegral y))
multiplyVariable (Int' x) (Flt y) = Right (Flt (fromIntegral x * y))
multiplyVariable (Int' x) (Int' y) = Right (Int' (x * y))
multiplyVariable v1 v2 = Left $ operationError "multiplication" v1 v2

divideVariable :: Variable -> Variable -> Either String Variable
divideVariable (Flt _) (Flt 0) = Left "Division by zero"
divideVariable (Flt _) (Int' 0) = Left "Divison by zero"
divideVariable (Int' _) (Flt 0) = Left "Divison by zero"
divideVariable (Int' _) (Int' 0) = Left "Divison by zero"
divideVariable (Flt x) (Flt y) = Right (Flt (x / y))
divideVariable (Flt x) (Int' y) = Right (Flt (x / fromIntegral y))
divideVariable (Int' x) (Flt y) = Right (Flt (fromIntegral x / y))
divideVariable (Int' x) (Int' y) = Right (Int' (x `div` y))
divideVariable v1 v2 = Left $ operationError "division" v1 v2

compareVariable :: Variable -> Variable -> Either String CompareState
compareVariable (Flt x) (Flt y) = Right $ compare' x y
compareVariable (Flt x) (Int' y) = Right $ compare' x (fromIntegral y)
compareVariable (Int' x) (Flt y) = Right $ compare' (fromIntegral x) y
compareVariable (Int' x) (Int' y) = Right $ compare' x y
compareVariable (Str x) (Str y) = Right $ compare' x y
compareVariable v1 v2 = Left $ "Cannot compare types \"" ++ typeName v1 ++ "\" and \"" ++ typeName v2 ++ "\"" 
