module SubsInterpreter
       ( runProg
       , Error (..)
       , Value(..)
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
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
          Map.fromList [ ("===", undefined)
                       , ("<", undefined)
                       , ("+", undefined)
                       , ("*", undefined)
                       , ("-", undefined)
                       , ("%", undefined)
                       , ("Array.new", arrayNew)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap = undefined

instance Applicative SubsM where
  pure = undefined
  (<*>) = undefined

instance Monad SubsM where
  return x = undefined
  f >>= m = undefined
  fail s = undefined


arrayNew :: Primitive
arrayNew [IntVal n] | n > 0 = return $ ArrayVal(take n $ repeat UndefinedVal)
arrayNew _ = fail ("Array.new called with wrong number of arguments")

modify :: (Env -> Env) -> SubsM ()
modify f = undefined

updateEnv :: Ident -> Value -> SubsM ()
updateEnv name val = undefined

getVar :: Ident -> SubsM Value
getVar name = undefined

getFunction :: FunName -> SubsM Primitive
getFunction name = undefined

evalExpr :: Expr -> SubsM Value
evalExpr expr = undefined

stm :: Stm -> SubsM ()
stm s = undefined

program :: Program -> SubsM ()
program (Prog prog) = undefined

runProg :: Program -> Either Error Env
runProg prog = undefined
