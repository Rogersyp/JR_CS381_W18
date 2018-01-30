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


-- Jeremy's 4. 
--      constructs a MiniLogo program that draws a staircase of n 
--      steps starting from (0,0).
--      define stair(xStart, yStart){
--          line(xStart, yStart, xStart, yStart + 1)     -- draw |
--          line(xStart, yStart + 1, xStart + 1, yStart) -- draw --
--      }
--
--
-- stair::Cmd
-- stair = Define "stair" ["xStart", "yStart"]
--        [
--         Call "line" [Ref "xStart", Ref "yStart", Ref "xStart", Add (Ref "yStart") (Ref "1")], 
--         Call "line" [Ref "xStart", Add (Ref "yStart") (Ref "1"), Add (Ref "xStart") (Ref "1"), Ref "yStart"] 
--        ]
-- -- How it works:
-- --      - draw a stair going up then right starting from (n-1)(n-1)
-- --      - recursivly draw a single stair until 0 is reached
-- --      - when 0 is reached, stop, lift pen up
-- steps :: Int -> Prog
-- steps n = if(n <= 0) 
--             then [Pen Up] 
--           else
--             [Call "stair" [Lit (pred n), Lit (pred n)]] ++ steps(pred n)


-- 4.	
-- 	steps :: Int -> Prog
--	edge_case when steps == 0
--	regular step: Starts at (val, val) --> recurses on predecessor of the value
--		      Pen gets put down after (0,0), and draws to the top from there. 

steps :: Int -> Prog
steps 0 = [Pen Up, Move (Lit 0, Lit 0), Pen Down]
steps val = steps (pred val) ++ [Move ((Lit (pred val)),
            (Lit val))] ++ [Move ((Lit val), (Lit val))]


