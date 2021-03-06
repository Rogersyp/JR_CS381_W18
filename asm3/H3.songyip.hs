-- Jeremy Fischer   932-447-681
-- Yipeng Song	    932-470-819
-- Peter Dorich     932-441-378
--
--
module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen mode) (_, (curX,curY)) = ( (mode, (curX, curY)), Nothing )
cmd (Move newX newY) (mode, (curX, curY)) = case mode of
                                              (Down) -> ( (Down, (newX, newY)), Just ((curX, curY), (newX, newY)) )
                                              (Up) -> ( (Up, (newX, newY)), Nothing)


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] startState = (startState, [])
prog cmds startState = extractCmds cmds (startState, [])

extractCmds :: [Cmd] -> (State, [Line]) -> (State, [Line])
extractCmds [] (state, lines) = (state, lines)
extractCmds (curCom:t) (state, lines) = extractCmds t (getNewState curCom state, appendLine curCom state lines)

-- Return List of lines [Line]
appendLine :: Cmd -> State -> [Line] -> [Line]
appendLine (Pen mode) _ oldLines = oldLines
appendLine (Move newX newY) (mode, (curX, curY)) oldLines = case mode of
                                                              (Down) -> oldLines ++ [((curX, curY), (newX, newY))]
                                                              (Up) -> oldLines

-- Return New State
getNewState :: Cmd -> State -> State
getNewState (Pen mode) (curMode, (curX, curY)) = (mode, (curX, curY))
getNewState (Move newX newY) (mode, (curX, curY))  = (mode, (newX, newY))


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = goTo (33, 24) ++
          [Move 30 24, Move 30 20, Move 33 20] ++
          goTo (38, 24) ++
          [Move 35 24, Move 35 22, Move 38 22, Move 38 20, Move 35 20] ++
          goTo (40, 24) ++
          [Move 43 24, Move 43 22, Move 40 22, Move 43 22, Move 43 20, Move 40 20] ++
          goTo (45, 22) ++
          [Move 45 24, Move 48 24, Move 48 22, Move 45 22, Move 45 20, Move 48 20, Move 48 22] ++
          goTo (50, 23) ++
          [Move 51 24, Move 51 20, Move 50 20, Move 52 20]


goTo :: (Int, Int) -> Prog
goTo (x, y) = [Pen Up, Move x y, Pen Down]      
