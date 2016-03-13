module Vulgr.Graph.Persist where

import Data.Graph.Inductive.Basic
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC

import Vulgr.Graph.Graphable


persist :: VulgrGraph g => g -> Either TC.TransError ()
persist g = do
    gr <- extract g
