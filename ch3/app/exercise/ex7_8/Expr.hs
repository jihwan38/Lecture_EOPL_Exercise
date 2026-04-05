{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expr where

type Program = Exp

-- [TODO] Complete data Exp. 
-- data Exp = 
--       ... 
data Exp = 
    Const_Exp Int
  | Diff_Exp Exp Exp
  | Add_Exp Exp Exp
  | Mul_Exp Exp Exp
  | Quot_Exp Exp Exp
  | IsZero_Exp Exp
  | IsEqual_Exp Exp Exp
  | IsGreater_Exp Exp Exp
  | IsLess_Exp Exp Exp
  | If_Exp Exp Exp Exp
  | Var_Exp Identifier
  | Let_Exp Identifier Exp Exp
  deriving Show

type Identifier = String

const_exp n = Const_Exp n
diff_exp e1 e2 = Diff_Exp e1 e2
add_exp e1 e2  = Add_Exp e1 e2
mul_exp e1 e2  = Mul_Exp e1 e2
quot_exp e1 e2 = Quot_Exp e1 e2

iszero_exp e   = IsZero_Exp e
isequal_exp e1 e2   = IsEqual_Exp e1 e2
isgreater_exp e1 e2 = IsGreater_Exp e1 e2
isless_exp e1 e2    = IsLess_Exp e1 e2

if_exp e1 e2 e3 = If_Exp e1 e2 e3

var_exp s = Var_Exp s

let_exp x e1 e2 = Let_Exp x e1 e2