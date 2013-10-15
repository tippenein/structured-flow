module Structured where

import Data.Map as Map

-- Decision points have a left and a right branch 
-- Left is False, Right is True
-- a flow graph can start with either a node or a decision
-- 
--        ----- {S1}
--        |      |
--        |    <cond>   _____
--        | F  /    \T |    |
--        ---{S3}  <cond>   |
--                 /    \   |
--               {S5}  {S6}--
--                 |
--                exit 7
--
data FromFile = FFExitNode Int  
              | FFStep Int Int  -- id, next id
              | FFBranch Int Int Int

data Flow = ExitNode Int
          | Step Int Flow
          | Branch Int Flow Flow
        deriving (Show)


mkFlow :: Map Int FromFile -> FromFile -> Flow
mkFlow ff start = go start Map.empty
  where
  -- Exit node
  go (FFExitNode exitid) _ = ExitNode exitid
  -- Normal Node case
  go (FFStep stepid nextid) flowMap =
    case Map.lookup stepid flowMap of
      Nothing ->
        -- insert the node id and tie the knot
        let step = Step stepid next
            next = go (ff ! nextid) $ Map.insert stepid step flowMap
        in step
      Just step -> step
  -- branch case
  go (FFBranch branchid leftid rightid) flowMap =
    case Map.lookup branchid flowMap of
      Nothing ->
        -- insert the condition id and tie the knot
        let branch = Branch branchid left right
            fm'    = Map.insert branchid branch flowMap
            left   = go (ff ! leftid) fm'
            right  = go (ff ! rightid) fm'
        in branch
      Just b -> b

-- generateStructured :: FlowGraph -> FlowGraph
-- generateStructured fg = Nothing

ffmFromList :: [FromFile] -> Map Int FromFile
ffmFromList ffl = Map.fromList $ [(getId n, n) | n <- ffl]
  where
    getId :: FromFile -> Int
    getId ff =
      case ff of
        FFStep n _ -> n
        FFBranch n _ _ -> n
        FFExitNode n -> n

main =
  mkFlow ffm (ffm ! 1)
  where
    ffm = ffmFromList ffl
    -- FromFile representation of the flowgraph drawn at the top of this file
    ffl = [(FFStep 1 2), (FFBranch 2 3 4), (FFStep 3 1), (FFBranch 4 5 6),
          (FFStep 5 7), (FFStep 6 4), (FFExitNode 7)]
