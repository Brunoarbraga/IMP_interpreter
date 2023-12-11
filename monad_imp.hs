{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- Imports
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Data.Function
import qualified Data.Map as Map


--------------------------------------------------------------------------
-- TYPE DECLARATIONS

-- Comandos da linguagem (if, while...)
data Com = CSkip
   -- | CAsgn String Exp
    | CSeq Com Com
    | CIf (Exp Bool) Com Com
    | CWhile (Exp Bool) Com
    | CPrint (Exp Int)
    | CRead String      



class TypeOf a where
  typeOf :: a -> Type a

instance TypeOf Int where
  typeOf _ = TInt

instance TypeOf Bool where
  typeOf _ = TBool



-- Expressions and operations
data Type :: * -> * where
  TInt    :: Type Int
  TBool   :: Type Bool
  TArrow  :: Type a -> Type b -> Type (a -> b)

data Exp :: * -> * where
    Lit :: TypeOf a => a -> Exp a
    Op :: Op a b -> (Exp a -> Exp a -> Exp b)

data Op :: * -> * -> * where
  APlus :: Op Int Int
  AMinus :: Op Int Int
  Amult :: Op Int Int
  BEq :: Op Int Bool
  BNeq :: Op Int Bool
  BLe :: Op Int Bool
  BGt :: Op Int Bool
  BAnd :: Op Bool Bool
  BOr :: Op Bool Bool
  BNot :: Op Bool Bool

-- Value type
data Value = IntVal Int
    | BoolVal Bool
    deriving(Show)

-- Environments
type Env = [(String , Value)]
type VarEnv = [String]


-- Monads
type InterpM a = ExceptT String (StateT Env IO) a
type VarM a = ExceptT String (StateT VarEnv IO) a  

--------------------------------------------------------------------------







--------------------------------------------------------------------------
-- INTERPRETER EVALUATORS

  

-- Função que interpreta a mônada
-- Recebe a mônada interpM, um estado inicial Env e retorna um IO com uma tupla de resultado
-- A tupla possui um Either com uma mensagem de erro ou um valor, e o estado final Env
runInterp :: InterpM a -> Env -> IO (Either String a, Env)
runInterp ev st = runStateT (runExceptT ev) st

-- Interpretador das expressões
interpExp :: Exp t -> InterpM t

interpExp (Lit n) = return n

interpExp (Op b e1 e2) =  do
    v1 <- interpExp e1
    v2 <- interpExp e2
    return (evalOp b v1 v2)

evalOp :: Op a b -> a -> a -> b
evalOp APlus = (+)
evalOp AMinus = (-)
evalOp Amult = (*)
evalOp BEq  = (==)
evalOp BLe  = (<)
evalOp BGt  = (>)
evalOp BAnd = (&&)
evalOp BOr  = (||)





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
        True -> interpCom com1
        False -> interpCom com2
interpCom (CPrint e1) = do
    v1 <- interpExp e1
    liftIO $ print v1
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




--data Type = IntType | BoolType deriving(Show)

-- Type checker environment, Keeps track of the types of each expression
--type TypeEnv = [(Exp Int, Type Int)]
type TypeEnv = [Int]

-- Type checker state monad 
type TypeM b = ExceptT String (StateT TypeEnv IO) b 


-- Type Checker run function:
-- takes a monad, a type environment, and returns an IO tuple with the result and an updated enviroment 
runTypeChecker :: VarM b -> VarEnv -> IO (Either String b, VarEnv)
runTypeChecker ev st = runStateT (runExceptT ev) st

-- Type checker function:
-- Takes an expression, if the expression has correct typing, updates the type environment with 
-- the expression and its type, otherwise throws and error
typeCheckerExp :: Exp t -> VarM (Type t)

typeCheckerExp (Lit n) = return (litType n) where
    litType :: TypeOf a => a -> Type a
    litType n = case typeOf n of
      TInt  -> TInt
      TBool -> TBool 

-- Checks the type of both expressions in the sum, if they are both Integers, the sum is considered correct.
typeCheckerExp (Op APlus e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    return TInt

-- Checks the type of both expressions in the subtraction, if they are both Integers, the subtraction is considered correct.
typeCheckerExp (Op AMinus e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TInt, TInt) -> return TInt

-- Checks the type of both expressions in the multiplication, if they are both Integers, the multiplication is considered correct.
typeCheckerExp (Op Amult e1 e2) = do
    type1 <- typeCheckerExp e1
    type2 <- typeCheckerExp e2
    case (type1, type2) of
        (TInt, TInt) -> return TInt

{-

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



-}





----------------------------------------------------------------------
-- INTERPRETER COMMAND TYPECHECKER
-- Manages scopes with a [String] variable environment


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

            -- Saves the current env to check both commands with it
            currentEnv <- get

            -- Checks com1 and saves the updated env
            typeCheckerCom com1 
            envAfterCom1 <- get

            -- Restores the previous env to check com2 with it
            put currentEnv
        
            -- Checks com2 and saves the updated env
            typeCheckerCom com2
            envAfterCom2 <- get

            -- Unites both envs and removes the duplicate variables
            put (removeDuplicates(envAfterCom1 ++ envAfterCom2))
            return ()
            --return $ removeDuplicates (result1 ++ result2)

typeCheckerCom (CWhile exp com) = do
    type1 <- typeCheckerExp exp
    case type1 of
        TBool -> do 
            typeCheckerCom com
            modify (\env -> removeDuplicates env)
            return ()
            --return $ removeDuplicates result


-- removes duplicate variables from variables list
removeDuplicates :: [String] -> [String]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

--------------------------------------------------------------------------










-- Example expression for simple tests
--exampleExp = (CSeq (CRead "var2") (CWhile (BAnd (BTrue) (BTrue)) (CRead "var"))) 
--BLe (ANum 12 `APlus` (AId "var")) (ANum 100)

