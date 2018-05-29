--Tulio Brandao Soares Martins ra:177761
--Luis Fernando Vieira Silva   ra:


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

--encontra todas as componentes conexas e as lista em uma lista de componentes
cC [] _ = []
cC (x:xs) edges
  | rest == [] = [components]
  | otherwise      = components : cC rest edges
  where
  rest   = (x:xs) \\ components
  components = dFS (x:xs) edges [x]

--executa o DFS para encontrar uma componente conexa
dFS _ _ [] = []
dFS v e (first:rest)
    | ([x|x<-v, x==first] == []) = dFS remain e rest
    | otherwise                  = first : dFS remain e (adj ++ rest)
    where
    adj = [x|(x, y, z)<-e, y == first] ++ [x|(y, x, z)<-e, y == first]
    remain = [x|x<-v, x/=first]

--Remove um item de uma lista de arestas
rmv :: Eq a => (a, a, Double) -> [(a, a, Double)] -> [(a, a, Double)]
rmv _ [] = []
rmv a (x:xs)
  | a == x    = rmv a xs
  | otherwise = x : rmv a xs

--Remove a maior aresta de uma lista de arestas
findMax :: Eq a =>  [(a, a, Double)] -> [(a, a, Double)] -> (a, a, Double) ->[(a, a, Double)]
findMax [] edges res  = rmv res edges
findMax ((a, b, c):xs) edges (x, y, mx)
    | c > mx    = findMax xs edges (a, b, c)
    | otherwise = findMax xs edges (x, y, mx)

--agrupa em clusters o grafo dado pelas arestas removendo as k maiores
--arestas e devolvendo uma lista de arestas atualizada
k_grouping :: Eq a => [(a, a, Double)] -> Int -> [(a, a, Double)]
k_grouping edg 0 = edg
k_grouping (x:xs) k = k_grouping (findMax (x:xs) (x:xs)  x) (k-1)

--devole a lista de vertices do GraphW
v_parser (GraphW vs es) = vs
--devole a lista de arestas do GraphW
e_parser (GraphW vs es) = es

main = do print $ cC graph_v (k_grouping graph_e 0)
    where
    graph = prim (GraphW [1,2,3,4,5] g5)
    graph_v = v_parser(graph)
    graph_e = e_parser(graph)
    g5 = [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93),(2,1,12),(3,1,34),(5,1,78),(4,2,55),(5,2,32),(4,3,61),(5,3,44),(5,4,93)]
