module HW3 where

import Render
import MiniMiniLogo


type State = (Mode, Point)
 
origin :: State
origin = (Up,(0,0))

cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen mode) (_, (curX,curY)) = ( (mode, (curX, curY)), Nothing )
cmd (Move newX newY) (mode, (curX, curY)) = case mode of
												(Down) -> ( (Down, (newX, newY)), Just ((curX, curY), (newX, newY)) )
												(Up) -> ( (Up, (newX, newY)), Nothing)

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
										(Down) -> oldLines ++ [Just ((curX, curY), (newX, newY))]
										(Up) -> oldLines

-- Return New State
getNewState :: Cmd -> State -> State
getNewState (Pen mode) (curMode, (curX, curY)) = (mode, (curX, curY))
getNewState (Move newX newY) (mode, (curX, curY))  = (mode, (newX, newY))