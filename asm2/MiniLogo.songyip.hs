module MiniLogo where

import Data.List
import Prelude hiding (Num)


-- 1. Define the abstract syntax of MiniLogo as a set of Haskell data types
type Num   = Int       -- any natural number
type Var   = String    -- any variable name 
type Macro = String    -- any macro name

type Prog  = [Cmd]     -- sequence of commands

data Mode  = Down      -- pen status
           | Up
           deriving (Show, Eq) 

data Expr  = Ref Var   -- variable reference
           | Lit Num   -- literal number
           | Add Expr Expr -- addition expression
           deriving (Eq, Show)

data Cmd   = Pen Mode                -- change pen mode
           | Move (Expr, Expr)       -- move pen to a new positon
           | Define Macro [Var] Prog -- define a macro
           | Call Macro [Expr]       -- invoke a macro    


-- 2. 
--      MiniLogo concrete syntax:
--      define line (x1, y1, x2, y2) {
--          pen up; move (x1, y1)
--          pen down; move (x2, y2)
--      }    	

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"]
       [Pen Up, Move (Ref "x1", Ref "y1"), Pen Down, Move (Ref "x2", Ref "y2")]



