import System.IO 
import Data.List
import Data.List.Split
import Debug.Trace

data Point = Point Int Int deriving (Show, Eq)
type Node = String
data Edge = Edge Node Node deriving (Show, Eq)
data Placement = Placement Point Node deriving (Show, Eq)
type Points = [Point]
type Graph = [Edge]
type Placements = [Placement]

startPoint :: Point -> Point
startPoint (Point x y)
	| x > 0 && y <= x && y >= -x = Point 1 0
	| x > 0 && y > x             = Point 0 1
	| x < 0 && y > -x            = Point 0 1
	| x < 0 && y <= -x && y >= x = Point (-1) 0
	| otherwise                  = Point 0 (-1)

clock :: [Point]
clock = cycle [Point x y | (x,y) <- [(0,1),(1,1),(1,0),(1,(-1)),(0,(-1)),((-1),(-1)),((-1),0),((-1),1)]]

clockwize :: Point -> Points
clockwize point = take 8 $ dropWhile (\p -> p /= point) clock

neighbours :: Point -> Points
neighbours point@(Point xx yy) = map (\(Point dx dy) -> Point (xx + dx) (yy + dy)) directed
	where
		start = startPoint point
		directed = (if xx >= 0 then id else reverse) $ clockwize start

isOccupied :: Placements -> Point -> Bool
isOccupied placements point = any (\(Placement placedPoint _) -> placedPoint == point) placements

isPlaced :: Placements -> Node -> Bool
isPlaced placements node = any (\(Placement _ placedNode) -> placedNode == node) placements

generatePoints :: Placements -> Graph -> Points
generatePoints _ [] = []
generatePoints [] _ = []
generatePoints ((Placement point node):ps) edges@(Edge fNode tNode:graph) =
	(if node == fNode then neighbours point else []) ++ generatePoints ps edges

place :: Placements -> Points -> Graph -> (Bool, Placements)
place placements _ [] = (True, placements)
place placements [] _ = (False, placements)
place placements (point:points) edges@(Edge fNode tNode:graph)
	| isOccupied placements point = (False, placements)
	| otherwise = if nextOk then (True, nextPlacements) else trace ("backtracking to place " ++ tNode) place placements points edges
	-- | otherwise = if nextOk then (True, nextPlacements) else place placements points edges
	where
		newPlacements = (Placement point tNode):placements
		newPoints = generatePoints newPlacements graph
		remainder = filter (\(Edge f t) -> t /= tNode) graph
		(nextOk, nextPlacements) = trace ("placed " ++ tNode ++ " at " ++ (show point) ++ ", now placed " ++ (show $ length newPlacements) ++ " and trying " ++ (show $ length newPoints)) place newPlacements newPoints remainder
		-- (nextOk, nextPlacements) = place newPlacements newPoints remainder

pointMult :: Int -> Point -> Point
pointMult f (Point x y) = Point (f * x) (f * y)

startPoints :: Points
startPoints = 
	let	c = cycle $ neighbours (Point 0 0)
		n = (take 8 $ repeat 4) ++ map (+6) n
	in zipWith pointMult n c

orderEdges :: [Node] -> Graph -> Graph
orderEdges [] graph = graph
orderEdges (node:nodes) graph =
	let	(outEdges, otherEdges) = partition (\(Edge fNode tNode) -> node == fNode) graph
		forwardNodes = map (\(Edge _ tNode) -> tNode) outEdges
		forwardEdges = orderEdges (forwardNodes ++ nodes) otherEdges
	in outEdges ++ forwardEdges

doPlace :: Graph -> Maybe Placements
doPlace graph = 
	let	hasPred (Edge fNode tNode) = any (\(Edge _ t) -> t == fNode) graph
		(midEdges, startEdges) = partition hasPred graph
		startNodes = nub $ map (\(Edge node _) -> node) startEdges
		initPlacements = zipWith (\point node -> Placement point node) startPoints startNodes
		orderedEdges = orderEdges startNodes graph
		-- _ = trace("orderedEdges = " ++ (show orderedEdges)) True
		(done, placements) = place initPlacements (generatePoints initPlacements orderedEdges) orderedEdges
	in if done then Just placements else Nothing

parseEdge :: String -> Edge
parseEdge line =
	let	fields = splitOn "\t" line
	in Edge (fields!!1) (fields!!2)

unparsePlacements :: Maybe Placements -> String
unparsePlacements Nothing = "No placements"
-- unparsePlacements (Just placements) = unlines $ map (\(Placement (Point x y) node) -> ("[" ++ node ++ " (" ++ (show x) ++ ", " ++ (show y) ++ ")]")) placements
unparsePlacements (Just placements) = unlines $ map (\(Placement (Point x y) node) -> (node ++ "\t" ++ (show x) ++ "\t" ++ (show y))) placements

process :: String -> String
process lns =
	let	rows = lines lns
		allEdges = map parseEdge rows
		edges = filter (\(Edge fNode tNode) -> fNode /= "base" && tNode /= "base") allEdges
		placements = doPlace edges
	in unparsePlacements placements

--main = interact process
main = do
	withFile "./map-data.tsv" ReadMode (\h -> do 
		contents <- hGetContents h
		putStr (process contents))
