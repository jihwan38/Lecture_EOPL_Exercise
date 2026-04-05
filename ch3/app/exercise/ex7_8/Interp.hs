{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interp where

import Expr
import Env

--
value_of :: Exp -> Env -> ExpVal

value_of (Const_Exp n) env = Num_Val n 

value_of (Diff_Exp exp1 exp2) env = 
    let val1 = value_of exp1 env
        val2 = value_of exp2 env
    in Num_Val (expval_num val1 - expval_num val2)

value_of (Add_Exp exp1 exp2) env = 
    let val1 = value_of exp1 env
        val2 = value_of exp2 env
    in Num_Val (expval_num val1 + expval_num val2)

value_of (Mul_Exp exp1 exp2) env = 
    let val1 = value_of exp1 env
        val2 = value_of exp2 env
    in Num_Val (expval_num val1 * expval_num val2)

value_of (Quot_Exp exp1 exp2) env = 
    let val1 = value_of exp1 env
        val2 = value_of exp2 env
    in Num_Val (quot (expval_num val1) (expval_num val2))

value_of (Var_Exp s) env = 
    apply_env env s

value_of (IsZero_Exp exp) env =
    let val1 = value_of exp env
        num1 = expval_num val1
    in if num1 == 0 then Bool_Val True
       else Bool_Val False

value_of (IsEqual_Exp exp1 exp2) env = 
    let val1 = value_of exp1 env
        val2 = value_of exp2 env
    in if expval_num val1 == expval_num val2 then Bool_Val True
       else Bool_Val False

value_of (IsGreater_Exp exp1 exp2) env =
    let val1 = value_of exp1 env
        val2 = value_of exp2 env
    in if expval_num val1 > expval_num val2 then Bool_Val True
       else Bool_Val False

value_of (IsLess_Exp exp1 exp2) env =
    let val1 = value_of exp1 env
        val2 = value_of exp2 env
    in if expval_num val1 < expval_num val2 then Bool_Val True
       else Bool_Val False

value_of (If_Exp exp1 exp2 exp3) env = 
    let val1 = value_of exp1 env
        bool1 = expval_bool val1
    in if bool1 then value_of exp2 env
       else value_of exp3 env

value_of (Let_Exp var exp1 body) env = 
    let val1 = value_of exp1 env
        new_env = extend_env var val1 env
    in value_of body new_env

--
value_of_program :: Exp -> ExpVal

value_of_program exp = value_of exp initEnv

-- [TODO] Complete initEnv.
--   { x |-> 10, v |-> 5, i |-> 1 }
initEnv :: Env
initEnv = extend_env "i" (Num_Val 1)
            (extend_env "v" (Num_Val 5)
              (extend_env "x" (Num_Val 10)
                empty_env))

          

