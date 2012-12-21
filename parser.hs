import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Maybe
import Control.Monad.Error

data LispVal = 
    Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Character Char
    | String String
    | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character contents) = show contents
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispError = 
    NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError
        
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

{-Parser-}
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> throwError $ Parser err
    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ chars 
    char '"'
    return $ String x
    where
        chars = escaped <|> noneOf "\""
        escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
        escapedChar code replacement = char code >> return replacement
        codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom 
        
parseBool :: Parser LispVal
parseBool = do
    x <- (char '#' >> oneOf "tf")
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber = do
    base <- (char '#' >> oneOf "odx") <|> return 'd'
    parseDigits base

parseDigits :: Char -> Parser LispVal
parseDigits base = do 
    num <- many1 d
    return $ (Number . fst . head . baseRead) num
    where 
        d = case base of
            'o' -> octDigit 
            'x' -> hexDigit 
            _   -> digit
        baseRead = case base of
            'o' -> readOct 
            'x' -> readHex 
            _   -> readDec

parseChar :: Parser LispVal
parseChar = do
    char '#'
    char '\\'
    x <- anyChar
    return $ Character x

parseLists :: Parser LispVal
parseLists = do
    char '('
    head <- sepEndBy parseExpr spaces
    tail <- liftM Just (char '.' >> spaces >> parseExpr) <|> return Nothing
    char ')'
    return $ case tail of
        Just e -> DottedList head e
        Nothing -> List head


parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = try parseNumber <|>
            try parseBool <|>
            try parseString <|>
            try parseChar <|> 
            try parseQuoted <|>
            try parseAtom <|>
            try parseLists                

{-Evaluation-}
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?" , unaryOp symbolp) ,
              ("string?" , unaryOp stringp) ,
              ("number?" , unaryOp numberp) ,
              ("bool?", unaryOp boolp) ,
              ("list?" , unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

main :: IO()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
                  
