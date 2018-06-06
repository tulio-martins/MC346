--Tulio Brandao Soares Martins ra:177761
--Luis Fernando Vieira Silva   ra:173170

import System.Environment
import System.IO
import Data.Bool
import Data.List
import Data.Ord

data GraphW a = GraphW [a] [(a, a, Double)]
    deriving (Show, Eq)

data Point a b = Point { name :: a
                       , position :: [b]
                       } deriving (Show, Eq)

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

-- encontra todas as componentes conexas e as lista em uma lista de componentes
cC [] _ = []
cC (x:xs) edges
  | rest == [] = [components]
  | otherwise      = components : cC rest edges
  where
    rest   = (x:xs) \\ components
    components = dFS (x:xs) edges [x]

-- executa o DFS para encontrar uma componente conexa
dFS _ _ [] = []
dFS v e (first:rest)
    | ([x|x<-v, x==first] == []) = dFS remain e rest
    | otherwise                  = first : dFS remain e (adj ++ rest)
    where
    adj = [x|(x, y, z)<-e, y == first] ++ [x|(y, x, z)<-e, y == first]
    remain = [x|x<-v, x/=first]

-- Remove um item de uma lista de arestas
rmv :: Eq a => (a, a, Double) -> [(a, a, Double)] -> [(a, a, Double)]
rmv _ [] = []
rmv a (x:xs)
  | a == x    = rmv a xs
  | otherwise = x : rmv a xs

-- Remove a maior aresta de uma lista de arestas
findMax :: Eq a =>  [(a, a, Double)] -> [(a, a, Double)] -> (a, a, Double) ->[(a, a, Double)]
findMax [] edges res  = rmv res edges
findMax ((a, b, c):xs) edges (x, y, mx)
    | c > mx    = findMax xs edges (a, b, c)
    | otherwise = findMax xs edges (x, y, mx)

-- agrupa em clusters o grafo dado pelas arestas removendo as k maiores
-- arestas e devolvendo uma lista de arestas atualizada
k_grouping :: Eq a => [(a, a, Double)] -> Int -> [(a, a, Double)]
k_grouping edg 0 = edg
k_grouping (x:xs) k = k_grouping (findMax (x:xs) (x:xs)  x) (k-1)

-- devole a lista de vertices do GraphW
v_parser (GraphW vs es) = vs
-- devole a lista de arestas do GraphW
e_parser (GraphW vs es) = es

-- Generates GraphW with connection from all vertices to each other
generatesGraph :: [(Point a Double)] -> [(Point a Double)] -> 
    (Point a Double) -> [a] -> [(a, a, Double)] -> (GraphW a)
generatesGraph [] _ _ accV accE = (GraphW accV accE)
generatesGraph ((Point n p):xs) [] _ accV accE = 
    (generatesGraph xs xs (Point n p) (n:accV) (accE))
generatesGraph vv ((Point n1 p1):ys) (Point n2 p2) accV accE =
     generatesGraph vv ys (Point n2 p2) accV 
     ((n2, n1, (distance (Point n1 p1) (Point n2 p2))):accE)

-- Calculates the distance between two points
distance :: (Floating b) => (Point a b) -> (Point a b) -> b
distance (Point _ position1) (Point _ position2) = 
    sqrt (foldl (+) 0 (map (^2) (zipWith (-) position1 position2)))

-- Give a vector of Strings representing the points, returns a vector of Points
getPoints :: [String] -> [Point [Char] Double]
getPoints [] = []
getPoints (x:xs) =
    let values = words x
        pName = head values
        pPosition = map (read::String->Double) (tail values)
    in (Point pName pPosition):(getPoints xs)


main = do
    input <- getContents
    let allLines = lines input
        numComponents = read (head allLines) :: Int
        points = getPoints (tail allLines)
        graph = generatesGraph points [] undefined [] [] 
        graphP = prim graph
        components = cC (v_parser graphP) (k_grouping (e_parser graphP) (numComponents-1))
        result = map sort components
    --print numComponents
    --mapM print points
    --print graph
    mapM putStrLn (map (intercalate " ") result)
