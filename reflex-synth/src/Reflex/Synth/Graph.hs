module Reflex.Synth.Graph where

import Reflex.Synth.Node

data GraphDef = GraphDef {
  nodeDefs :: [NodeDef],
  connections :: [(Int,Int)]
}

data Graph = Graph {
  nodes :: [Node]
}

nodeDefToGraphDef :: NodeDef -> GraphDef
nodeDefToGraphDef n =

instantiateGraph :: GraphDef -> IO Graph
instantiateGraph x = do
  y <- mapM createNodeFromDef $ nodeDefs x -- IO [Node]
  mapM_ (\(a,b) -> connectNodes (y!!a) (y!!b)) $ connections x
  return $ Graph y

startGraph :: Graph -> IO ()
startGraph g = mapM_ startNode $ nodes g
