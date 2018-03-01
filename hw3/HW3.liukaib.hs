-- Homework #3
-- Due: Wed, Feb 13, 10:59pm

-- Team members:
---- Kaibo Liu (liukaib)
---- Qibang Liu (liuqib)
---- Donghao Lin (lindo)
-- 02/11/2018


module HW3 (State, cmd, prog) where

import Render
import MiniMiniLogo


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
--   >>> cmd (Pen Up) (Down, (0, 0))
--   ((Up,(0,0)),Nothing)
--
--   >>> cmd (Move 1 1) (Down, (0, 0))
--   ((Down,(1,1)),Just ((0,0),(1,1)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen a) (_, (x, y))        = ((a, (x, y)), Nothing)
cmd (Move x2 y2) (mode, (x1, y1)) = case mode of
                                        Up   -> ((mode, (x2, y2)), Nothing)
                                        Down -> ((mode, (x2, y2)), Just ((x1, y1), (x2, y2)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
--
--   >>> prog [] (Up, (0, 4))
--   ((Up,(0,4)),[])
--
--   >>> prog [(Pen Up)] (Down, (0, 0))
--   ((Up,(0,0)),[])
--
--   >>> prog [(Move 1 1)] (Down, (0, 0))
--   ((Down,(1,1)),[((0,0),(1,1))])
--
--   >>> prog [(Move 1 2), (Pen Up)] (Down, (0, 0))
--   ((Up,(1,2)),[((0,0),(1,2))])
--
--   >>> prog [(Move 1 2), (Move 1 3)] (Down, (0, 0))
--   ((Down,(1,3)),[((0,0),(1,2)),((1,2),(1,3))])
--
prog :: Prog -> State -> (State, [Line])
prog [] state   = (state, [])
prog cmds state = applyCmds cmds (state, [])


--
-- * Helper Functions
--

--
-- | Recursively applies a list of commands while updating pen state and accummulating drawn lines.
--
--   >>> applyCmds [(Pen Up)] ((Down, (0, 1)), [])
--   ((Up,(0,1)),[])
--
--   >>> applyCmds [(Move 2 2)] ((Down, (0, 0)), [])
--   ((Down,(2,2)),[((0,0),(2,2))])
--
--   >>> applyCmds [(Move 2 2), (Move 3 3)] ((Down, (1, 0)), [((0,0),(2,2))])
--   ((Down,(3,3)),[((0,0),(2,2)),((1,0),(2,2)),((2,2),(3,3))])
--
--   >>> applyCmds [Pen Up,Move 10 10,Pen Down,Move 15 17,Pen Up,Move 10 17,Pen Down,Move 15 10] ((Up, (0, 0)), [])
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
applyCmds :: [Cmd] -> (State, [Line]) -> (State, [Line])
applyCmds [] result               = result
applyCmds (c:cmds) (state, lines) = case line of
                                        Just l  -> applyCmds cmds (newState, lines ++ [l])
                                        Nothing -> applyCmds cmds (newState, lines)
                                    where (newState, line) = cmd c state


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
--
--   Draws "CS381"
--
-- >>> prog amazing (Up, (0, 0))
-- ((Down,(16,20)),[((3,23),(0,23)),((0,23),(0,20)),((0,20),(3,20)),((7,23),(4,23)),((4,23),(4,22)),((4,22),(7,22)),((7,22),(7,20)),((7,20),(4,20)),((8,23),(11,23)),((11,23),(11,22)),((11,22),(8,22)),((8,22),(11,22)),((11,22),(11,20)),((11,20),(8,20)),((15,22),(15,23)),((15,23),(12,23)),((12,23),(12,22)),((12,22),(15,22)),((15,22),(15,20)),((15,20),(12,20)),((12,20),(12,22)),((16,23),(16,20))])
--
-- >>> draw amazing
--
--
amazing :: Prog
amazing = --liftPen(2,1) ++ [Move 2 7] ++ liftPen(5,1) ++ [Move 2 4, Move 5 7] ++   -- K
--          liftPen(6,1) ++ [Move 6 4, Move 8 7, Move 9 4, Move 9 1] ++ liftPen(6,4) ++ [Move 9 4] ++       -- A
--          liftPen(10,1) ++ [Move 12 1] ++ liftPen(10,7) ++ [Move 12 7] ++liftPen(11,1) ++ [Move 11 7] ++ -- I
--          liftPen(13,1) ++ [Move 13 7, Move 16 7, Move 16 6, Move 15 5, Move 13 5, Move 13 4, Move 15 4, Move 16 3, Move 16 1, Move 13 1] ++ -- B
--          liftPen(17,2) ++ [Move 17 6, Move 18 7, Move 19 7, Move 20 6, Move 20 2, Move 19 1, Move 18 1, Move 17 2]++  -- O
--          liftPen(23,7) ++ [Move 23 1, Move 26 1] ++       -- L
--          liftPen(27,1) ++ [Move 29 1] ++ liftPen(27,7) ++ [Move 29 7] ++liftPen(28,1) ++ [Move 28 7] ++ -- I
--          liftPen(30,7) ++ 
--          [Move 30 1, Move 32 1, Move 33 2, Move 33 1, Move 33 7] ++      -- U
          -- OR map
          liftPen (10, 30)++
          [Move 11 30, Move 12 30, Move 13 30, Move 14 31, Move 14 30, Move 15 30, Move 16 30, Move 17 30, Move 17 31, Move 18 31, Move 18 30, Move 19 31, Move 20 31, Move 21 31, Move 21 32, Move 22 32, Move 23 32, Move 24 32, Move 28 32, Move 30 32, Move 31 32, Move 32 32, Move 33 32, Move 33 31, Move 34 31, Move 34 30, Move 34 29, Move 34 28, Move 33 28, Move 33 27, Move 33 26, Move 32 25, Move 32 24, Move 31 24, Move 31 23, Move 31 22, Move 32 22, Move 33 22, Move 33 21, Move 32 21, Move 32 20, Move 32 19, Move 32 9, Move 31 9, Move 30 9, Move 27 9, Move 26 9, Move 25 9, Move 24 9, Move 23 9, Move 22 9, Move 21 9, Move 20 9, Move 19 9, Move 17 9, Move 16 9, Move 15 9, Move 14 9, Move 12 9, Move 11 9, Move 10 9, Move 9  9, Move 8  9, Move 7  9, Move 6  9, Move 5  9, Move 4  9, Move 3  9, Move 2  9, Move 2  10, Move 2  11, Move 2  12, Move 1  12, Move 2  12, Move 2  13, Move 1  13, Move 1  14, Move 1  15, Move 2  15, Move 2  16, Move 2  17, Move 2  18, Move 3  19, Move 3  20, Move 3  21, Move 3  22, Move 3  23, Move 3  24, Move 3  25, Move 3  26, Move 3  27, Move 3  28, Move 3  29, Move 4  29, Move 3  29, Move 4  30, Move 4  31, Move 3  31, Move 4  31, Move 3  31, Move 3  32, Move 4  32, Move 4  33, Move 3  33, Move 3  34, Move 4  34, Move 4  33, Move 4  34, Move 5  34, Move 6  34, Move 6  33, Move 7  33, Move 8  33, Move 8  32, Move 9  32, Move 8  32, Move 9  31, Move 9  30, Move 10 30] ++
          -- OSU logo
          liftPen(52, 34) ++
          [Move 48 34, Move 44 33, Move 41 31, Move 41 11, Move 44 9, Move 48 8, Move 52 8, Move 53 8, Move 55 8, Move 56 9, Move 58 9, Move 60 10, Move 61 10, Move 63 11, Move 63 11, Move 57 13, Move 55 13, Move 53 13, Move 52 13, Move 50 13, Move 47 13, Move 46 14, Move 45 14, Move 45 28, Move 46 28, Move 47 29, Move 50 30, Move 52 30, Move 54 30, Move 56 29, Move 58 28, Move 58 20, Move 58 20, Move 62 19, Move 63 19, Move 63 31, Move 60 33, Move 56 34, Move 52 34]++
          liftPen(64, 28) ++
          [Move 64 23, Move 66 23, Move 68 23, Move 69 22, Move 69 20, Move 74 20, Move 74 25, Move 71 26, Move 67 28, Move 64 28]++
          liftPen(57, 27) ++
          [Move 55 26, Move 54 26, Move 52 25, Move 52 16, Move 53 16, Move 70 10, Move 70 7, Move 69 7, Move 67 6, Move 65 5, Move 63 5, Move 61 5, Move 59 6, Move 57 7, Move 57 8, Move 56 7, Move 54 7, Move 52 7, Move 52 4, Move 56 2, Move 59 1, Move 63 1, Move 67 1, Move 71 2, Move 74 4, Move 74 14, Move 74 14, Move 57 19, Move 57 27] ++
          -- OR character
          liftPen(11,16) ++ [Move 11 22, Move 12 23, Move 15 23, Move 16 22, Move 16 16, Move 15 15, Move 12 15, Move 11 16] ++ -- O
          liftPen(19,15) ++ [Move 19 23, Move 23 23, Move 24 22, Move 24 21, Move 23 20, Move 19 20, Move 24 15]   -- R 


--          liftPen (30,1) ++
--          [Move 32 8, Move 32 9, Move 32 10, Move 32 10, Move 32 10, Move 31 10, Move 31 11, Move 32 11, Move 32 12, Move 33 13, Move 33 13, Move 33 14, Move 34 15, Move 35 16] ++
--          [Move 35 16, Move 34 17, Move 34 18, Move 31 18, Move 28 19, Move 25 19, Move 25 19, Move 24 19, Move 23 19, Move 23 19, Move 22 19, Move 21 19, Move 21 20, Move 20 20, Move 20 20, Move 19 20, Move 19 20, Move 17 20, Move 16 21, Move 16 21, Move 16 22, Move 16 23, Move 15 23, Move 15 24, Move 15 24, Move 13 24, Move 13 22, Move 12 20, Move 12 19, Move 11 16, Move 9  13, Move 8  11, Move 7  10, Move 7  9, Move 7  6, Move 30 1, Move 30 1] ++          


--
-- * Helper Functions
--

--
-- | Returns a set of commands that moves the pen to the given coordinates without drawing a line,
--   then puts the pen down.
--
--   >> liftPen (1, 0)
--   [Pen Up,Move 1 0,Pen Down]
--
--   >> prog (liftPen (1, 0)) (Up, (7, 7))
--   ((Down,(1,0)),[])
--
liftPen :: (Int, Int) -> Prog
liftPen (x, y) = [Pen Up, Move x y, Pen Down]
