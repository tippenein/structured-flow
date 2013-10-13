module Structured where

-- a string value and a Node that it points to.
data Node = Node { value :: String
                 , next :: Node
                 } deriving Show
-- Decision points have a left and a right branch 
-- Left is False, Right is True
data DecisionP = DecisionP { t :: Node
                           , f :: Node
                           } deriving Show

-- a flow graph can start with either a node or a decision
data FlowGraph = FlowGraph Node | DecisionP

--        _______
--        |      |
--        |    <cond>   _____
--        | F  /    \T |    |
--        ---{S3}  <cond>   |
--                 /    \   |
--               {S4}  {S5}--
--                 |
--                end
--

-- How to represent DAGs ?!

generateStructured :: FlowGraph -> FlowGraph
generateStructured fg = Nothing
