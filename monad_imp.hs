-- Imports
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

-- Comandos da linguagem (if, while...)
data Com = CSkip
    | CAsgn String Exp
    | CSeq Com Com
    | CIf Exp Com Com
    | CWhile Exp Com
    | CPrint Exp
    | CRead String
    deriving(Show)      
        
-- Expressões aritiméticas e booleanas
data Exp = ANum Integer
    | AId String
    | APlus Exp Exp
    | AMinus Exp Exp
    | Amult Exp Exp
    | BTrue
    | BFalse 
    | BEq Exp Exp
    | BNeq Exp Exp
    | BLe Exp Exp
    | BGt Exp Exp
    | BNot Exp
    | BAnd Exp Exp
    deriving(Show)

data Value = IntVal Integer
    | BoolVal Bool
    deriving(Show)

-- Environment
type Env = [(String , Value)] 

-- Mônada: ExceptT retorna uma String de erro ou outra mônada, no caso StateT. StateT recebe um estado inicial,
--no caso, o Env, e uma mônada padrão IO
type InterpM a = ExceptT String (StateT Env IO) a   

-- Função que interpreta a mônada
--Recebe a mônada interpM, um estado inicial Env e retorna um IO com uma tupla de resultado
--A tupla possui um Either com uma mensagem de erro ou um valor, e o estado final Env
runInterp :: InterpM a -> Env -> IO (Either String a, Env)
runInterp ev st = runStateT (runExceptT ev) st

-- Interpretador das expressões
interpExp :: Exp -> InterpM Value
-- Se a expressão for um número, usa o contrutor IntVal pra transformar esse número para o tipo Value
interpExp (ANum n) = return $ IntVal n
-- Interpreta as duas expressões, olha se o valor resultante de ambas são inteiros, e faz a soma
interpExp (APlus e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> return $ IntVal (v1 + v2)
        (IntVal v1, BoolVal v2) -> throwError ("Error: Mismatched types for arithmetic sum. Second expression is boolean, must be Integer")
        (BoolVal v1, IntVal v2) -> throwError ("Error: Mismatched types for arithmetic sum. First expression is boolean, must be Integer")
        _ -> throwError ("Error: Mismatched types for arithmetic sum. Both expressions are boolean, must be Integer")
-- Interpreta as duas expressões, olha se o valor resultante de ambas são inteiros, e faz a subtração
interpExp (AMinus e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> return $ IntVal (v1 - v2)
        (IntVal v1, BoolVal v2) -> throwError ("Error: Mismatched types for arithmetic subtraction. Second expression is boolean, must be Integer")
        (BoolVal v1, IntVal v2) -> throwError ("Error: Mismatched types for arithmetic subtraction. First expression is boolean, must be Integer")
        _ -> throwError ("Error: Mismatched types for arithmetic subtraction. Both expressions are boolean, must be Integer")
-- Interpreta as duas expressões, olha se o valor resultante de ambas são inteiros, e faz a multiplicação
interpExp (Amult e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> return $ IntVal (v1 * v2)
        (IntVal v1, BoolVal v2) -> throwError ("Error: Mismatched types for arithmetic multiplication. Second expression is boolean, must be Integer")
        (BoolVal v1, IntVal v2) -> throwError ("Error: Mismatched types for arithmetic multplication. First expression is boolean, must be Integer")
        _ -> throwError ("Error: Mismatched types for arithmetic multiplication. Both expressions are boolean, must be Integer")
-- Se a expressão for um booleano, usa o contrutor BoolBal pra transformar esse booleano para o tipo Value
interpExp (BFalse) = return $ BoolVal False
interpExp (BTrue) = return $ BoolVal True
interpExp (BEq e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1,v2) of
        (IntVal v1, IntVal v2) -> if v1 == v2 then return $ BoolVal True else return $ BoolVal False
        (BoolVal v1, BoolVal v2) -> if v1 == v2 then return $ BoolVal True else return $ BoolVal False
        _ -> throwError ("Error: Cannot compare boolean expressions with arithmetic expressions")
interpExp (BNeq e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1,v2) of
        (IntVal v1, IntVal v2) -> if v1 == v2 then return $ BoolVal False else return $ BoolVal True
        (BoolVal v1, BoolVal v2) -> if v1 == v2 then return $ BoolVal False else return $ BoolVal True
        _ -> throwError ("Error: Cannot compare boolean expressions with arithmetic expressions")
interpExp (BNot e1) = do
    v1 <- interpExp e1
    case v1 of
        (BoolVal v1) -> return $ BoolVal (not v1)
        _ -> throwError ("Error: Boolean operator 'not' cannot be used on arithmetic expressions")
interpExp (BAnd e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (BoolVal v1, BoolVal v2) -> return $ BoolVal (v1 && v2)
        (IntVal v1, BoolVal v2) -> throwError ("Error: Mismatched types for boolean expression 'and'. First expression is Integer, must be boolean.")
        (BoolVal v1, IntVal v2) -> throwError ("Error: Mismatched types for boolean expression 'and'. Second expression is Integer, must be boolean.")
        _ -> throwError ("Error: Mismatched types for boolean expression 'and'. Both expressions are Integer, must be boolean.")
interpExp (BLe e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> if v1 < v2 then return $ BoolVal True else return $ BoolVal False
        _ -> throwError ("Error: ")
interpExp (BGt e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> if v1 > v2 then return $ BoolVal True else return $ BoolVal False
        _ -> throwError ("Error: ")
interpExp (AId s) = lookupVar s


-- Interpretador dos comandos
--Recebe um comando, e como os comandos não retonam nada, só executam ações,
interpCom :: Com -> InterpM ()
interpCom (CSkip) = return ()
interpCom (CSeq com1 com2) = do
    interpCom com1
    interpCom com2
interpCom (CIf e1 com1 com2) = do
    v1 <- interpExp e1
    case v1 of
        (BoolVal True) -> interpCom com1
        (BoolVal False) -> interpCom com2
        _ -> throwError ("Conditional expression must be a boolean expression")
interpCom (CPrint e1) = do
    v1 <- interpExp e1
    case v1 of
        (IntVal v1) -> liftIO $ print v1 
interpCom (CRead var) = do
    x <- liftIO readInteger
    modify (\env -> (var, IntVal x) : env)
    return ()

-- Lê um valor do teclado e o retorna
readInteger :: IO Integer
readInteger = read <$> getLine
    
-- Lookup the value of a variable inside the environment
lookupVar :: String -> InterpM Value
lookupVar s = do
    env <- lift get
    case lookup s env of
        Just v -> return v
        Nothing -> throwError ("Undefined variable")



exampleExp = (CRead "var") 
--BLe (ANum 12 `APlus` (AId "var")) (ANum 100)
