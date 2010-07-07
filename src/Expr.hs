module Expr where

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State

data Expr = LispInt Int |
            LispSymbol String |
            LispList [Expr] |
            LispFn LispResult FunctionSig |
            LispSpecial LispResult FunctionSig

type FunctionSig = [String]
type LispError = ErrorT String IO
type LispResult = StateT Context LispError Expr

type SymbolTable = Map.Map String Expr
data Context = Ctx SymbolTable (Maybe Context)

instance Show Expr where
    show (LispInt x) = show x
    show (LispSymbol x) = x
    show (LispList x) = "(" ++ unwords (map show x) ++ ")"
    show (LispFn _ _) = "<function>"
    show (LispSpecial _ _) = "<special>"

eval :: Expr -> LispResult
eval (LispInt n) = return (LispInt n)
eval (LispFn f args) = return (LispFn f args)
eval (LispSpecial f args) = return (LispSpecial f args)
eval (LispSymbol s) = do context <- get
                         lookup context
    where lookup (Ctx table parent) =
              if s `Map.member` table == True
              then return (table Map.! s)
              else case parent of
                     Nothing -> throwError ("Don't know symbol " ++ s)
                     (Just p) -> lookup p
-- eval (LispList (x:xs)) = do fn <- eval x
--                             apply fn
--     where apply (LispSpecial f expectedArgs) = apply' expectedArgs xs f
--           apply (LispFn f expectedArgs) = do args <- mapM eval xs
--                                              apply' expectedArgs args f