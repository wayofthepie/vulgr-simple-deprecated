{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Vulgr.Graph.Graphable (
    UniqueNodeGraph
    , getGraph

    , Graphable
    , GNodeData
    , GRelationData
    , graph
    , emptyUNGraph
    , consEdge
    , consNode
    , pretty
    ) where

import Data.Functor
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import Data.Monoid ((<>))
import qualified Data.Text as T
import Debug.Trace

-- | A graph with unique nodes.
--
-- These are currently the only types of graphs vulgr cares about.
--
-- The type of node data is 'n' and the type of relationship data
-- is 'r'. A hashmap is used to track the uniqueness of nodes.
--
data UniqueNodeGraph n r = UniqueNodeGraph
    { nodeMap :: H.HashMap n Int
    , unGraph :: Gr n r
    } deriving (Eq, Show)

getGraph :: UniqueNodeGraph n r -> Gr n r
getGraph = unGraph

-- | The constraints a node must have to be graphable.
--
type NodeConstraints n = (Eq n, Hashable n, Show n)


-- | A UniqueNodeGraphable specification data type s where 'n' is the type of its node data and
-- 'r' is the type of its relationship data.
--
-- To infer the types of the nodes and relationships we need to tell the compiler
-- that 'n' is equivalent 'GNodeData s' and 'r' is equivalent to 'GRelationData s'.
--
class (n ~ GNodeData s, r ~ GRelationData s, NodeConstraints n) => Graphable s n r where
    type GNodeData s     :: *
    type GRelationData s :: *
    graph :: s -> UniqueNodeGraph (GNodeData s) (GRelationData s)


-- | Construct an empty unique node graph.
--
emptyUNGraph :: NodeConstraints n => UniqueNodeGraph n r
emptyUNGraph = UniqueNodeGraph H.empty empty


-- | Construct and edge between the node at 'n1' and the node at 'n2'. The 'relationData'
-- tracks metadata about the relationship - labels, properties, direction etc...
--
consEdge :: NodeConstraints n => Int -> Int -> r -> UniqueNodeGraph n r -> UniqueNodeGraph n r
consEdge n1 n2 relationData (UniqueNodeGraph m g) =
    UniqueNodeGraph m (insEdge (n1, n2, relationData) g)


-- | Construct a new node with data 'n' in the graph. Returns the graph and the int id
-- of the new node.
--
consNode :: NodeConstraints n => n -> UniqueNodeGraph n r -> (Int, UniqueNodeGraph n r)
consNode ndata (UniqueNodeGraph m g) = create ndata $ H.lookup ndata m
  where
    -- | Create a node in the graph if it does not already exist.
    create ndata (Just nid) = (nid, UniqueNodeGraph m (insNode (nid, ndata) g))
    create ndata Nothing =
        let [newNid] = newNodes 1 g
            m'       = traceShow (show newNid) $ H.insert ndata newNid m
        in  (newNid, UniqueNodeGraph m' (insNode (newNid, ndata) g))


-- | Convenience method to pretty print the graph.
--
pretty ::(Show r, NodeConstraints n) => UniqueNodeGraph n r -> T.Text
pretty g = T.pack . prettify $ getGraph g
