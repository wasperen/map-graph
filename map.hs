import System.IO 
import Data.List
import Data.List.Split
import Debug.Trace
import Data.Maybe

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

data Line = Line Point Point deriving (Show, Eq)

graphLines :: Graph -> Placements -> [Line]
graphLines [] _ = []
graphLines edges placements = 
	let	findFPlacement (Edge (Node fName _) _) = find (\(Placement point (Node name _)) -> name == fName) placements
		findTPlacement (Edge _ (Node tName _)) = find (\(Placement point (Node name _)) -> name == tName) placements
	in [Line fPoint tPoint |
		edge <- edges,
		let fPlacement = findFPlacement edge, isJust fPlacement, let (Placement tPoint _) = fromJust fPlacement,
		let tPlacement = findTPlacement edge, isJust tPlacement, let (Placement fPoint _) = fromJust tPlacement]

linesCross :: Line -> Line -> Bool
linesCross (Line a b) (Line c d) =
	(ccw a c d) == (ccw b c d) && (ccw a b c) == (ccw a b d)
	where
		ccw (Point ax ay) (Point bx by) (Point cx cy) = ((cy - ay) * (bx - ax)) > ((by - ay) * (cx - ax))

isGoodPlacement :: Graph -> Placements -> Placement -> Bool
isGoodPlacement graph placements (Placement point@(Point x y) node) = 
	(not $ isOccupied placements point) &&
	(all (== False) [linesCross a b | a <- existingLines, b <- newLines])
	where
		existingLines = graphLines graph placements
		newLines = [Line point otherPoint |
			(Placement otherPoint placedNode) <- placements,
			any (\(Edge fNode tNode) -> fNode == placedNode && tNode == node) graph]

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

unparsePlacedGraph :: Graph -> [Placements] -> String
unparsePlacedGraph _ [] = "No placements"
unparsePlacedGraph graph ([]:placementss) = unparsePlacedGraph graph placementss
unparsePlacedGraph graph (placements:_) =
	let	findPlacement node = fromJust $ find (\(Placement _ n) ->  n == node) placements
		printLine (Placement (Point fX fY) (Node fNode fColor)) (Placement (Point tX tY) (Node tNode tColor)) = intercalate "\t" [fNode, fColor, (show fX), (show fY), tNode, tColor, (show tX), (show tY)]
		printEdge edge@(Edge fNode tNode) = printLine (findPlacement fNode) (findPlacement tNode)
	in (intercalate "\t" ["fNodeName","fColor","fX","fY","tNodeName","tColor","tX","tY"]) ++ "\n" ++ (unlines $ map printEdge graph)

process :: String -> String
process lns =
	let	rows = lines lns
		allEdges = map parseEdge rows
		edges = filter (\(Edge (Node fNode _) (Node tNode _)) -> fNode /= "base" && tNode /= "base") allEdges
		placementss = doPlace edges
	in unparsePlacedGraph edges placementss

--main = interact process
main = do
	withFile "./map-data.tsv" ReadMode (\h -> do 
		contents <- hGetContents h
		putStr (process contents))
