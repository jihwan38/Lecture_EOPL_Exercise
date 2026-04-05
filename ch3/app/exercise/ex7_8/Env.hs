{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Env where

import Expr

-- [TODO] Complete data Env.
--   data Env = 
--     ...
data Env = 
    Empty_Env
  | Extend_Env Identifier ExpVal Env
  deriving (Show)


empty_env :: Env
empty_env = Empty_Env

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env x v env = Extend_Env x v env

apply_env :: Env -> Identifier -> ExpVal
apply_env Empty_Env searched_var = error (searched_var ++ "is not found.")
apply_env (Extend_Env x v env) searched_var
  | x == searched_var = v
  | otherwise         = apply_env env searched_var

-- [TODO] Complete data ExpVal.
--   data ExpVal = 
--     ...
data ExpVal = 
    Num_Val {expval_num :: Int} 
  | Bool_Val {expval_bool :: Bool}  
   
instance Show ExpVal where
   show (Num_Val num)   = show num
   show (Bool_Val bool) = show bool

type DenVal = ExpVal   