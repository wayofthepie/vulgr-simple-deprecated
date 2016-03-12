module Vulgr.Graph.Persist where

import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC

import Vulgr.Graph.Graphable


class Graphable s => Persistable s where
    persist :: GraphType s -> Either TC.TransError ()

