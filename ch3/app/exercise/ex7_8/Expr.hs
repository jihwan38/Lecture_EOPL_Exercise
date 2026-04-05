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
  | IsZero_Exp Exp
  | If_Exp Exp Exp Exp
  | Var_Exp Identifier
  | Let_Exp Identifier Exp Exp
  deriving Show

type Identifier = String

const_exp n = Const_Exp n
diff_exp e1 e2 = Diff_Exp e1 e2

iszero_exp e = IsZero_Exp e

if_exp e1 e2 e3 = If_Exp e1 e2 e3

var_exp s = Var_Exp s

let_exp x e1 e2 = Let_Exp x e1 e2