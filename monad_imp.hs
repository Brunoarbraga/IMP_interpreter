-- Imports
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map


--------------------------------------------------------------------------
-- TYPE DECLARATIONS

-- Comandos da linguagem (if, while...)
data Com = CSkip
   -- | CAsgn String Exp
    | CSeq Com Com
    | CIf Exp Com Com
    | CWhile Exp Com
    | CPrint Exp
    | CRead String
    deriving(Show)      
        
-- Expressões aritiméticas e booleanas
data Exp = ANum Int
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

data Value = IntVal Int
    | BoolVal Bool
    deriving(Show)

-- Environment
type Env = [(String , Value)] 


-- Mônada: ExceptT retorna uma String de erro ou outra mônada, no caso StateT. StateT recebe um estado inicial,
--no caso, o Env, e uma mônada padrão IO
type InterpM a = ExceptT String (StateT Env IO) a 

--------------------------------------------------------------------------







--------------------------------------------------------------------------
-- INTERPRETER EVALUATORS

  

-- Função que interpreta a mônada
--Recebe a mônada interpM, um estado inicial Env e retorna um IO com uma tupla de resultado
--A tupla possui um Either com uma mensagem de erro ou um valor, e o estado final Env
runInterp :: InterpM a -> Env -> IO (Either String a, Env)
runInterp ev st = runStateT (runExceptT ev) st

-- Interpretador das expressões
interpExp :: Exp -> InterpM Value

-- Se a expressão for um número, usa o contrutor IntVal pra transformar esse número para o tipo Value
interpExp (ANum n) = return $ IntVal n

-- Interpreta as duas expressões e realiza a soma
interpExp (APlus e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> return $ IntVal (v1 + v2)

-- Interpreta as duas expressões, olha se o valor resultante de ambas são inteiros, e faz a subtração
interpExp (AMinus e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> return $ IntVal (v1 - v2)

-- Interpreta as duas expressões, olha se o valor resultante de ambas são inteiros, e faz a multiplicação
interpExp (Amult e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> return $ IntVal (v1 * v2)

-- Se a expressão for um booleano, usa o contrutor BoolVal pra transformar esse booleano para o tipo Value
interpExp (BFalse) = return $ BoolVal False
interpExp (BTrue) = return $ BoolVal True
interpExp (BEq e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1,v2) of
        (IntVal v1, IntVal v2) -> if v1 == v2 then return $ BoolVal True else return $ BoolVal False
        (BoolVal v1, BoolVal v2) -> if v1 == v2 then return $ BoolVal True else return $ BoolVal False

interpExp (BNeq e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1,v2) of
        (IntVal v1, IntVal v2) -> if v1 == v2 then return $ BoolVal False else return $ BoolVal True
        (BoolVal v1, BoolVal v2) -> if v1 == v2 then return $ BoolVal False else return $ BoolVal True

interpExp (BNot e1) = do
    v1 <- interpExp e1
    case v1 of
        (BoolVal v1) -> return $ BoolVal (not v1)
  
interpExp (BAnd e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (BoolVal v1, BoolVal v2) -> return $ BoolVal (v1 && v2)
  
interpExp (BLe e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> if v1 < v2 then return $ BoolVal True else return $ BoolVal False

interpExp (BGt e1 e2) = do
    v1 <- interpExp e1
    v2 <- interpExp e2
    case (v1, v2) of
        (IntVal v1, IntVal v2) -> if v1 > v2 then return $ BoolVal True else return $ BoolVal False

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
        _ -> throwError("Expression to be printed must result in an Int") 
interpCom (CRead var) = do
    x <- liftIO readInteger
    modify (\env -> (var, IntVal x) : env)
    return ()

-- Lê um valor do teclado e o retorna
readInteger :: IO Int
readInteger = read <$> getLine
    
-- Lookup the value of a variable inside the environment
lookupVar :: String -> InterpM Value
lookupVar s = do
    env <- lift get
    case lookup s env of
        Just v -> return v
        Nothing -> throwError ("Undefined variable")

--------------------------------------------------------------------------









--------------------------------------------------------------------------
-- INTERPRETER EXPRESSION TYPE CHECKER



-- 
data Type = TInt | TBool deriving(Show)

-- Type checker environment, Keeps track of the types of each expression
type TypeEnv = [(Exp , Type)]

-- Type checker state monad 
type TypeM b = ExceptT String (StateT TypeEnv IO) b 


-- Type Checker run function:
-- takes a monad, a type environment, and returns an IO tuple with the result and an updated enviroment 
runTypeChecker :: VarM b -> VarEnv -> IO (Either String b, VarEnv)
runTypeChecker ev st = runStateT (runExceptT ev) st

-- Type checker function:
-- Takes an expression, if the expression has correct typing, updates the type environment with 
-- the expression and its type, otherwise throws and error
typeCheckerExp :: Exp -> VarM Type

-- A num is an Int, so returns TInt type
typeCheckerExp (ANum num) = return TInt

-- Checks the type of both expressions in the sum, if they are both Integers, the sum is considered correct.
typeCheckerExp (APlus e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TInt, TInt) -> return TInt
        _ -> throwError("Incompatible types for expression addition")

-- Checks the type of both expressions in the subtraction, if they are both Integers, the subtraction is considered correct.
typeCheckerExp (AMinus e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TInt, TInt) -> return TInt
        _ -> throwError("Incompatible types for expression subtraction")

-- Checks the type of both expressions in the multiplication, if they are both Integers, the multiplication is considered correct.
typeCheckerExp (Amult e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TInt, TInt) -> return TInt
        _ -> throwError("Incompatible types for expression multiplication")
    
-- BTrue and BFalse are booleans, so returns TBool type
typeCheckerExp (BTrue) = return TBool
typeCheckerExp (BFalse) = return TBool

-- BEq checks the type of both expressions, if they are equal, the equality is considered correct
typeCheckerExp (BEq e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TInt, TInt) -> return TInt
        (TBool, TBool) -> return TBool
        _ -> throwError("Incompatible types for expression equality") 

-- BNEq checks the type of both expressions, if they are equal, the inequality is considered correct
typeCheckerExp (BNeq e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TInt, TInt) -> return TInt
        (TBool, TBool) -> return TBool
        _ -> throwError("Incompatible types for expression inequality") 

typeCheckerExp (BLe e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TInt, TInt) -> return TInt
        _ -> throwError("Incompatible types for expression comparisson")

typeCheckerExp (BGt e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TInt, TInt) -> return TInt
        _ -> throwError("Incompatible types for expression comparisson")

typeCheckerExp (BNot e1) = do
    type1 <- typeCheckerExp e1
    case type1 of
        TBool -> return TBool
        _ -> throwError("Incompatible type for expression negation")

typeCheckerExp (BAnd e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TBool, TBool) -> return TBool
        _ -> throwError("Incompatible type for logical AND")

----------------------------------------------------------------------









----------------------------------------------------------------------
-- INTERPRETER COMMAND TYPECHECKER
-- Manages scopes with a [String] variable environment



type VarEnv = [String]
type VarM c = ExceptT String (StateT VarEnv IO) c 

typeCheckerCom :: Com -> VarM ()

typeCheckerCom (CSkip) = return () --[]

typeCheckerCom (CRead var) = do
    modify (\env -> var : env)
    return ()
    --return $ var : [] 

typeCheckerCom (CSeq com1 com2) = do
    typeCheckerCom com1 
    typeCheckerCom com2
    modify (\env -> removeDuplicates env)
    return ()
    --return $ removeDuplicates (result1 ++ result2)

typeCheckerCom (CIf exp com1 com2) = do
    type1 <- typeCheckerExp exp
    case type1 of 
        TBool -> do
            typeCheckerCom com1 
            typeCheckerCom com2
            modify (\env -> removeDuplicates env)
            return ()
            --return $ removeDuplicates (result1 ++ result2)
        _ -> throwError("Conditional expression must be boolean")

typeCheckerCom (CWhile exp com) = do
    type1 <- typeCheckerExp exp
    case type1 of
        TBool -> do 
            typeCheckerCom com
            modify (\env -> removeDuplicates env)
            return ()
            --return $ removeDuplicates result
        _ -> throwError("Conditional expression must be boolean")



-- removes duplicate variables from variables list
removeDuplicates :: [String] -> [String]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

--------------------------------------------------------------------------










-- Example expression for simple tests
exampleExp = (CSeq (CRead "var2") (CWhile (BAnd (BTrue) (BTrue)) (CRead "var"))) 
--BLe (ANum 12 `APlus` (AId "var")) (ANum 100)
