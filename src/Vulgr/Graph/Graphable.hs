{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Vulgr.Graph.Graphable {-(
    UniqueNodeGraph
    , getGraph

    , Graphable
    , GNodeData
    , GRelationData
    , GraphType
    , graph
    , emptyUNGraph
    , consEdge
    , consNode
    , pretty
    )-} where

import Data.Functor
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import Data.Monoid ((<>))
import qualified Data.Text as T
import Debug.Trace

-- | The constraints a node must have to be graphable.
--
type NodeConstraints n = (Eq n, Hashable n, Show n)


-- | A Graphable specification data type s where 'n' is the type of its node data and
-- 'r' is the type of its relationship data.
--
-- To infer the types of the nodes and relationships we need to tell the compiler
-- that 'n' is equivalent 'GNodeData s' and 'r' is equivalent to 'GRelationData s'.
--
class Graphable s where
    type GNodeData s     :: *
    type GRelationData s :: * -- not needed yet
    type GResult s :: *
    graph :: (VulgrGraph (GResult s), NodeConstraints (GNodeData s))
          => s
          -> GResult s


-- | The class of graph types we care about in Vulgr.
class VulgrGraph g where
    type VNodeData g :: *
    type VRelationData g :: *
    emptyGraph :: g
    consEdge   :: Int -> Int -> VRelationData g -> g -> g
    consNode   :: NodeConstraints (VNodeData g) => VNodeData g -> g -> (Int, g)
    extract    :: g -> Gr (VNodeData g) (VRelationData g)


-- | A graph with unique nodes.
--
-- The type of node data is 'n' and the type of relationship data
-- is 'r'. A hashmap is used to track the uniqueness of nodes.
--
data UniqueNodeGraph n r = UniqueNodeGraph
    { nodeMap :: H.HashMap n Int
    , unGraph :: Gr n r
    } deriving (Eq, Show)

instance VulgrGraph (UniqueNodeGraph n r) where
    type VNodeData (UniqueNodeGraph n r) = n
    type VRelationData (UniqueNodeGraph n r) = r


    -- | Construct an empty unique node graph.
    --
    emptyGraph = UniqueNodeGraph H.empty empty


    -- | Construct an edge between the node at 'n1' and the node at 'n2'. The 'relationData'
    -- tracks metadata about the relationship - labels, properties, direction etc...
    --
    consEdge n1 n2 relationData (UniqueNodeGraph m g) =
        UniqueNodeGraph m (insEdge (n1, n2, relationData) g)


    -- | Construct a new node with data 'n' in the graph. Returns the graph and the int id
    -- of the new node.
    --
    consNode ndata (UniqueNodeGraph m g) = create ndata $ H.lookup ndata m
      where
        -- | Create a node in the graph if it does not already exist.
        create ndata (Just nid) = (nid, UniqueNodeGraph m (insNode (nid, ndata) g))
        create ndata Nothing =
            let [newNid] = newNodes 1 g
                m'       = H.insert ndata newNid m
            in  (newNid, UniqueNodeGraph m' (insNode (newNid, ndata) g))

    extract = unGraph




-- | Convenience method to pretty print the graph.
--
pretty :: (Show r, NodeConstraints n) => UniqueNodeGraph n r -> T.Text
pretty g = T.pack . prettify $ extract g
