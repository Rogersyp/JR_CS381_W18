
-- Jeremy Fischer 932-447-681
-- Peter Dorich   932-441-378
-- Yipeng Song    932-470-819

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
      -- -- | Environment queries.
      -- data Test = Not    Test   -- boolean negation
      --           | Facing Card   -- am I facing the given cardinal direction?
      --           | Clear  Dir    -- can I move in the given relative direction?
      --           | Beeper        -- is there a beeper here?
      --           | Empty         -- is my beeper bag empty?
test :: Test -> World -> Robot -> Bool
test (Not tst) w r = not (test tst w r)
test (Facing card) _ r = card == getFacing r 
test (Clear dir) w r = isClear (relativePos dir r) w
test (Beeper) w r = hasBeeper (getPos r) w
test (Empty) _ r = isEmpty r




-- | Valuation function for Stmt.
      -- | Statements.
      -- data Stmt = Shutdown                 -- end the program
      --           | Move                     -- move forward
      --           | PickBeeper               -- take a beeper
      --           | PutBeeper                -- leave a beeper
      --           | Turn    Dir              -- rotate in place
      --           | Call    Macro            -- invoke a macro
      --           | Iterate Int  Stmt        -- fixed repetition loop
      --           | If      Test Stmt Stmt   -- conditional branch
      --           | While   Test Stmt        -- conditional loop
      --           | Block   [Stmt]           -- statement block
stmt :: Stmt -> Defs -> World -> Robot -> Result
-- simply shutdown the robot
stmt Shutdown   _ _ r = Done r

-- If the forward facing position is clear, then set the robot's
-- position to the postion ahead of it, otherwise error
stmt Move _ w r = if test (Clear Front) w r
                        then OK w (setPos (relativePos Front r) r)
                        else Error ("Blocked at: " ++ show (relativePos Front r))

-- If there is a beeper in this position, then decrement
-- the number of beepers in this position and increment the bag
-- ,otherwise error
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)

-- If the bag is not empty, then leave a beeper here and increment
-- the number of beepers in this position and decrement the bag
-- ,otherwise error
stmt PutBeeper _ w r = let p = getPos r
                        in if not (isEmpty r)
                              then OK (incBeeper p w) (decBag r)
                              else Error ("No beeper to put.") 

-- Simply turn the robot to the dir direction
stmt (Turn dir) _ w r = OK w (setFacing (cardTurn dir (getFacing r)) r)  

-- Call a Macro using the lookup function
stmt (Call m)   d w r = case lookup m d of
                          (Just b) -> stmt b d w r
                          _        -> Error ("Undefined macro: " ++ m)

--ITERATE:
stmt (Iterate i s) d w r = if i > 0 then case stmt s d w r of 
                                          (OK w' r') -> stmt (Iterate (i - 1) s) d w' r'
                                          (Done r')  -> Done r'
                                          (Error e)  -> Error e
                                    else OK w r


-- Blocks:
stmt (Block []) _ w r = OK w r
stmt (Block (s:ss)) d w r = case stmt s d w r of
                              (OK w' r') -> stmt (Block ss) d w' r'
                              (Done r')  -> Done r'
                              (Error e)  -> Error e

-- Loops:
stmt (While t s) d w r = if (test t w r) then case stmt s d w r of
                                                (OK w' r') -> stmt (While t s) d w' r'
                                                (Done r')  -> Done r'
                                                (Error e)  -> Error e
                                         else OK w r

--If (Test Stmt Stmt) -- conditional branch -- (if clear front?)
   --Need to use test function as basis for if stmt. 
stmt (If t x y)  d w r = if test t w r then stmt x d w r
                                       else stmt y d w r


    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
