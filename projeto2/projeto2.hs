
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

-- The graph from the problem statement:
g5 = [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93),(2,1,12),(3,1,34),(5,1,78),(4,2,55),(5,2,32),(4,3,61),(5,3,44),(5,4,93)]


--Removing the k biggest edges in the graph

--Removes an item from the list returning the updated list
rmv _ [] = []
rmv a (x:xs)
  | a == x    = rmv a xs
  | otherwise = x : rmv a xs

--Removes the biggest edge in the tree
findMax [] edges res  = rmv res edges
findMax ((a, b, c):xs) _ (x, y, mx)
    | c > mx    = findMax xs (a, b, c)
    | otherwise = findMax xs (x, y, mx)

--Performs findMax k times
k_grouping g5 0 = g5
k_grouping x:xs k = k_grouping (findMax g5 g5 x) k-1

--Given the edges and vertixes of a graph, finds its connected compenents

--conComponents edges vertixes result
conComponents _ [] res = res
conComponents edges x:xs resx:resxs
  | (belongs x resx) = conComponents edges xs resx:resxs
  | otherwise        =

belongs x [] = False
belongs x y:ys
  | x == y    = True
  | otherwise = belongs x ys



main = do print $ prim (GraphW [1,2,3,4,5] g5)
