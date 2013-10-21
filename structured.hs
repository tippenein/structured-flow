module Structured where

import Data.Map as M

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
data Flow = FExitNode Int
          | FStep Int Int  -- id, next id
          | FBranch Int Int Int

data FlowGraph = ExitNode Int
               | Step Int FlowGraph
               | Branch Int FlowGraph FlowGraph


mkFlow :: Map Int Flow -> Flow -> FlowGraph
mkFlow fm start = go start M.empty
  where
  -- Exit node
  go (FExitNode exitid) _ = ExitNode exitid
  -- Normal Node case
  go (FStep stepid nextid) flowMap =
    case M.lookup stepid flowMap of
      Nothing ->
        -- insert the node id and tie the knot
        let step = Step stepid next
            next = go (fm ! nextid) $ M.insert stepid step flowMap
        in step
      Just step -> step
  -- branch case
  go (FBranch branchid leftid rightid) flowMap =
    case M.lookup branchid flowMap of
      Nothing ->
        -- insert the condition id and tie the knot
        let branch = Branch branchid left right
            fm'    = M.insert branchid branch flowMap
            left   = go (fm ! leftid) fm'
            right  = go (fm ! rightid) fm'
        in branch
      Just b -> b

structure :: FlowGraph -> FlowGraph
structure fg = fg

fmFromList :: [Flow] -> Map Int Flow
fmFromList flowlist = M.fromList $ [(getId n, n) | n <- flowlist]
  where
    getId :: Flow -> Int
    getId ff =
      case ff of
        FStep n _ -> n
        FBranch n _ _ -> n
        FExitNode n -> n

main =
  mkFlow fm (fm ! 1)
  where
    fm = fmFromList fl
    -- Flow representation of the flowgraph drawn at the top of this file
    fl = [(FStep 1 2), (FBranch 2 3 4), (FStep 3 1), (FBranch 4 5 6),
          (FStep 5 7), (FStep 6 4), (FExitNode 7)]
