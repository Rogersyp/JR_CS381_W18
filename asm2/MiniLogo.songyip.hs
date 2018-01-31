-- Jeremy Fischer 932-447-681
-- Peter Dorich 932-441-378

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



-- 3. 
--      MiniLogo concrete syntax:
--      define nix (x1, y1, w, h) {
--          line(x1, y1, x1 + w, y1 + h) -- draw /
--          line(x1, y1 + h, x1 + w, y1) -- draw \
--      }    
nix::Cmd
nix = Define "nix" ["x1", "y1", "w", "h"]
       [
        Call "line" [Ref "x1", Ref "y1", Add (Ref "x1") (Ref "w"), Add (Ref "y1") (Ref "h")], 
        Call "line" [Ref "x1", Add (Ref "y1") (Ref "h"), Add (Ref "x1") (Ref "w"), Ref "y1"] 
       ]



-- 4.	
-- 	steps :: Int -> Prog
--	edge_case when steps == 0
--	regular step: Starts at (val, val) --> recurses on predecessor of the value
--		      Pen gets put down after (0,0), and draws to the top from there. 

steps :: Int -> Prog
steps 0 = [Pen Up, Move (Lit 0, Lit 0), Pen Down]
steps val = steps (pred val) ++ [Move ((Lit (pred val)),
            (Lit val))] ++ [Move ((Lit val), (Lit val))]



-- 5.
-- Returns a list of the names of all of the macros that are defined anywhere in a given MiniLogo program.
--
-- How it works: Traverse the list of commands,
--          If the current commad matches 'Define name _ _'
--          then it is a macro so return a list with the name in it.
--          If not, then return an empty list.
--          Traverse the reset of the list
checkMacro::Cmd -> [String]
checkMacro (Define name _ _) = [name]
checkMacro (_) = []

macros :: Prog -> [Macro]
macros [] = []
macros (x:t) = checkMacro x ++ macros t
