import System.IO 
import Data.List
import Data.List.Split
import Debug.Trace

data Point = Point Int Int deriving (Show, Eq)
data Node = Node String String deriving (Show, Eq)
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

isGoodPlacement :: Graph -> Placements -> Placement -> Bool
isGoodPlacement graph placements (Placement point node) = not $ isOccupied placements point

generatePoints :: Placements -> Edge -> Points
generatePoints [] _ = []
generatePoints ((Placement point node):ps) edge@(Edge fNode tNode) =
	(if node == fNode then neighbours point else []) ++ generatePoints ps edge

pointMult :: Int -> Point -> Point
pointMult f (Point x y) = Point (f * x) (f * y)

startPoints :: Points
startPoints = 
	let	c = cycle $ neighbours (Point 0 0)
		n = (take 8 $ repeat 1) ++ map (+3) n
	in zipWith pointMult n c

orderEdges :: [Node] -> Graph -> Graph
orderEdges [] graph = graph
orderEdges (node:nodes) graph =
	let	(outEdges, otherEdges) = partition (\(Edge fNode tNode) -> node == fNode) graph
		forwardNodes = map (\(Edge _ tNode) -> tNode) outEdges
		forwardEdges = orderEdges (forwardNodes ++ nodes) otherEdges
	in outEdges ++ forwardEdges

placeNode :: Graph -> Placements -> Node -> Points -> [Placements]
placeNode graph placements node points = [placement:placements | placement <- [(Placement point node) | point <- points], isGoodPlacement graph placements placement]

placeInitialNodes :: [Node] -> Points -> [Placements]
placeInitialNodes nodes points = [zipWith (\node point -> Placement point node) n points | n <- permutations nodes]

place :: Graph -> [Placements] -> [Edge] -> [Placements]
place _ placementss [] = placementss
place _ [] _ = []
place graph (placements:placementss) (edge@(Edge fNode tNode):edges) =
	let	points = generatePoints placements edge
		newPlacementss = placeNode graph placements tNode points
	in place graph newPlacementss edges ++ place graph placementss (edge:edges)

doPlace :: Graph -> [Placements]
doPlace graph = 
	let	hasNoPred (Edge fNode tNode) = not $ any (\(Edge _ t) -> t == fNode) graph
		startEdges = filter hasNoPred graph
		startNodes = nub $ map (\(Edge node _) -> node) startEdges
		initPlacementss = placeInitialNodes startNodes startPoints
		orderedEdges = orderEdges startNodes graph
	in place graph initPlacementss orderedEdges

parseEdge :: String -> Edge
parseEdge line =
	let	fields = splitOn "\t" line
		fNode = Node (fields!!1) (fields!!3)
		tNode = Node (fields!!2) (fields!!3)
	in Edge fNode tNode

unparsePlacements :: [Placements] -> String
unparsePlacements [] = "No placements"
unparsePlacements ([]:placementss) = unparsePlacements placementss
unparsePlacements (placements:_) = 
	let printLine (Placement (Point x y) (Node name color)) = name ++ "\t" ++ color ++ "\t" ++ (show x) ++ "\t" ++ (show y)
	in "nodeName\tcolor\txPos\tyPos\n" ++ (unlines $ map printLine placements)

process :: String -> String
process lns =
	let	rows = lines lns
		allEdges = map parseEdge rows
		edges = filter (\(Edge (Node fNode _) (Node tNode _)) -> fNode /= "base" && tNode /= "base") allEdges
		placements = doPlace edges
	in unparsePlacements placements

--main = interact process
main = do
	withFile "./map-data.tsv" ReadMode (\h -> do 
		contents <- hGetContents h
		putStr (process contents))
