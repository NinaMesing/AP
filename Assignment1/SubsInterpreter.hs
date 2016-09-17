module SubsInterpreter
       ( runProg
       , Error (..)
       , Value(..)
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)

-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Error = Error String
             deriving (Show, Eq)

type Env = Map Ident Value
type Primitive = [Value] -> SubsM Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", compareEqual)
                       , ("<", compareLessThan)
                       , ("+", addition)
                       , ("*", multiplication)
                       , ("-", subtraction)
                       , ("%", modulus)
                       , ("Array.new", arrayNew)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap f (SubsM g) = SubsM $ \c -> case g c of
                                       Left e -> Left e
                                       Right (a, env) -> Right (f a, env) 

instance Applicative SubsM where
    pure a = SubsM $ \c -> Right (a, fst c)
    (SubsM g) <*> b = SubsM $ \c -> case g c of
                                       Left e -> Left e
                                       Right (a, env) -> runSubsM (fmap a b) (env, snd c)

instance Monad SubsM where
  return x = SubsM $ \c -> Right (x, fst c)
  m >>= f = SubsM $ \c -> case runSubsM m c of
                             Left e -> Left e
                             Right (x, env) -> runSubsM (f x) (env, snd c)
  fail s = SubsM $ \c -> Left (Error s)

-- -- -- -- -- The initialContext functions -- -- -- -- -- --
arrayNew :: Primitive
arrayNew [IntVal n] | n > 0 = return $ ArrayVal(take n $ repeat UndefinedVal)
arrayNew _ = fail ("Array.new called with wrong number of arguments")

--We only compare intvals and stringvals
compareEqual :: Primitive
compareEqual [IntVal a, IntVal b] = return $ if a == b then TrueVal else FalseVal
compareEqual [StringVal a, StringVal b] = return $ if a == b then TrueVal else FalseVal
compareEqual _ = fail ("compareEqual called with wrong number of arguments or wrong type")

compareLessThan :: Primitive
compareLessThan [IntVal a, IntVal b] = return $ if a < b then TrueVal else FalseVal
compareLessThan [StringVal a, StringVal b] = return $ if a < b then TrueVal else FalseVal
compareLessThan _ = fail ("compareLessThan called with wrong number of arguments or wrong type")

addition :: Primitive
addition [IntVal a, IntVal b] = return $ IntVal (a+b)
addition [StringVal a, StringVal b] = return $ StringVal (a ++ b)
addition [IntVal a, StringVal b] = return $ StringVal (show a ++ b)
addition [StringVal a, IntVal b] = return $ StringVal (a ++ show b)
addition _ = fail ("addition called with wrong number of arguments or wrong types")

multiplication :: Primitive
multiplication [IntVal a, IntVal b] = return $ IntVal (a*b)
multiplication _ = fail ("multiplication called with wrong number of arguments or wrong type")

subtraction :: Primitive
subtraction [IntVal a, IntVal b] = return $ IntVal (a-b)
subtraction _ = fail ("subtraction called with wrong number of arguments or wrong type")

modulus :: Primitive
modulus [IntVal a, IntVal b] = return $ IntVal (mod a b)
modulus _ = fail ("modulus called with wrong number of arguments or wrong type")

-- -- -- -- -- The initialContext functions END -- -- -- -- -- --


modify :: (Env -> Env) -> SubsM ()
modify f = SubsM $ \c -> Right ((), f (fst c))

updateEnv :: Ident -> Value -> SubsM ()
updateEnv name val = modify (let f _ = Just val
                             in Map.alter f name) 

getVar :: Ident -> SubsM Value
getVar name = SubsM $ \c -> case Map.lookup name (fst c) of
                               Nothing -> Left (Error "Variable not found")
                               (Just value) -> Right (value, fst c)

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM $ \c -> case Map.lookup name (snd c) of
                                    Nothing -> Left (Error "function name not found")
                                    (Just value) -> Right (value, fst c)

evalExpr :: Expr -> SubsM Value
evalExpr (Number int) = return (IntVal int)
evalExpr (String s) = return (StringVal s)
evalExpr (Array xs) = do 
                        vals <- mapM (\x -> evalExpr x) xs
                        return $ ArrayVal vals 
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Var ident) = return (StringVal ident)
--evalExpr Compr ArrayFor Expr
evalExpr (Call funname exps) = do
                                f <- getFunction funname
                                vals <- mapM (\x -> evalExpr x) exps 
                                result <- f vals
                                return result
evalExpr (Assign id exp) = do 
                            a <- evalExpr exp
                            b <- updateEnv id a
                            return a
evalExpr (Comma e1 e2) = do 
                          a <- evalExpr e1
                          a <- evalExpr e2
                          return a

stm (VarDecl ident exp) = case exp of
                             Nothing -> fail("hej")
                             (Just expr) -> do
                                             a <- evalExpr expr
                                             result <- updateEnv ident a
                                             return result 
stm (ExprAsStm exp) = do
                       a <- evalExpr exp
                       return ()

--mapM mapper over monadetype, så vi kan gøre noget på værdierne. 
--Underscore betyder at den kun returnerer én værdi...
program :: Program -> SubsM ()
program (Prog prog) = mapM_ stm prog

runProg :: Program -> Either Error Env
runProg prog =  let x = program prog 
                in case runSubsM x (initialContext) of
                      Left e -> Left (Error "There was an error somewhere")
                      Right ((), env) -> Right env                   
