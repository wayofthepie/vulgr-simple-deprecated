{-# LANGUAGE TypeFamilies #-}
module Vulgr.Graph.Cypher where

import Control.Monad
import qualified Data.Text as T

data Cypher a = Cypher a | EmptyQuery
data CypherQuery  = CypherQuery { query :: T.Text }
data CypherItem = Neo4jNode | Neo4jRelation
data Bind a = Bind


instance Monad Cypher where
    EmptyQuery >>= f = EmptyQuery
    Cypher (CypherQuery q) >>= f =

bind "a" (Node p l)

match (bind "a" (Node props labels), bind "b" (Node props labels))
    merge (Node props labels) >>=
