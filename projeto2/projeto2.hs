
import Data.Bool
import Data.List
import Data.Ord

data GraphW a = GraphW [a] [(a, a, Int)]
    deriving (Show, Eq)


-- Initialise a graph (tree) with the first vertex, and begin:
prim (GraphW vs es) = prim' [head vs] [] (length vs - 1) (GraphW vs es)

-- Terminate when no vertices remain to be chosen:
prim' chosenV chosenE 0 _ = GraphW chosenV chosenE
prim' chosenV chosenE nE (GraphW vs es) = prim' (newV : chosenV) (newE : chosenE) (nE - 1) (GraphW vs es)
    where
    -- The edges we can pick from are extending outward from the tree:
    edges = filter (\(a, b, _) -> (a `elem` chosenV) `xor` (b `elem` chosenV)) es
    -- the new edge, newE, is the minimum weight one that connects our tree with a vertex
    -- not on the tree:
    newE @ (a', b', w') = minimumBy (comparing (\(_, _, w) -> w)) edges
    -- the new vertex is, therefore, the end of newE which isn't part of the tree so far:
    newV = if a' `elem` chosenV then b' else a'


xor :: Bool -> Bool -> Bool
xor = (/=)

--Removing the k biggest edges in the graph

cC [] _ = []
cC (x:xs) edges
  | rest == [] = [components]
  | otherwise      = components : cC rest edges
  where
  rest   = (x:xs) \\ components
  components = dFS (x:xs) edges [x]

dFS _ _ [] = []
dFS v e (first:rest)
  | ([x|x<-v, x==first] == []) = dFS remain e rest
  | otherwise                  = first : dFS remain e (adj ++ rest)
  where
  adj = [x|(x, y, z)<-e, x == first] ++ [x|(y, x, z)<-e, y == first]
  remain = [x|x<-v, x/=first]

--Removes an item from the list returning the updated list
rmv :: Eq a => (a, a, Double) -> [(a, a, Double)] -> [(a, a, Double)]
rmv _ [] = []
rmv a (x:xs)
  | a == x    = rmv a xs
  | otherwise = x : rmv a xs

--Removes the biggest edge in the tree
findMax :: Eq a =>  [(a, a, Double)] -> [(a, a, Double)] -> (a, a, Double) ->[(a, a, Double)]
findMax [] edges res  = rmv res edges
findMax ((a, b, c):xs) edges (x, y, mx)
    | c > mx    = findMax xs edges (a, b, c)
    | otherwise = findMax xs edges (x, y, mx)


k_grouping :: Eq a => [(a, a, Double)] -> Int -> [(a, a, Double)]
k_grouping edg 0 = edg
k_grouping (x:xs) k = k_grouping (findMax (x:xs) (x:xs)  x) (k-1)

v_parser (GraphW vs es) = vs
e_parser (GraphW vs es) = es

main = do print $ CC graph_v graph_e
    where
    graph = prim (GraphW buildGraphV buildGraphE)
    graph_v = v_parser(graph)
    graph_e = e_parser(graph)
