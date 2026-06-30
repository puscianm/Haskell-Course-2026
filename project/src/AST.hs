module AST
  ( Op (..),
    Value (..),
    Distribution (..),
    Expr (..),
    Statement (..),
    Program (..),
  )
where

data Op = Add | Sub | Mul | Div | Eq | Lt | And | Or
  deriving (Show, Eq)

data Value = RealV Double | IntV Int | BoolV Bool
  deriving (Show, Eq)

data Distribution = Distribution String [Expr]
  deriving (Show, Eq)

data Expr
  = Var String
  | Lit Value
  | BinOp Op Expr Expr
  | If' Expr Expr Expr
  deriving (Show, Eq)

data Statement
  = Sample String Distribution 
  | Let String Expr 
  | Observe String Expr 
  | If Expr [Statement] [Statement] 
  | Return [String] 
  deriving (Show, Eq)

data Program = Program [Statement]
  deriving (Show, Eq)
