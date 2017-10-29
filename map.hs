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

-- neighbours :: Point -> Points
-- neighbours (Point x y) = [Point (x + xx) (y + yy) | 
-- 	xx <- [(-1)..1],
-- 	yy <- [(-1)..1], 
-- 	not (xx == 0 && yy == 0)]

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
	| otherwise = if nextOk then (True, nextPlacements) else place placements points edges
	where
		newPlacements = (Placement point tNode):placements
		newPoints = generatePoints newPlacements graph
		(nextOk, nextPlacements) = trace ("place " ++ (show $ length newPlacements) ++ " " ++ (show newPoints)) place newPlacements newPoints graph

pointMult :: Int -> Point -> Point
pointMult f (Point x y) = Point (f * x) (f * y)

startPoints :: Points
startPoints = 
	let c = cycle $ neighbours (Point 0 0)
	in zipWith pointMult [1..] c

doPlace :: Graph -> Maybe Placements
doPlace graph = 
	let	hasPred (Edge fNode tNode) = any (\(Edge _ t) -> t == fNode) graph
		(midEdges, startEdges) = partition hasPred graph
		startNodes = nub $ map (\(Edge node _) -> node) startEdges
		initPlacements = zipWith (\point node -> Placement point node) (neighbours (Point 0 0)) startNodes
		(done, placements) = place initPlacements (generatePoints initPlacements graph) graph
	in if done then Just placements else Nothing

parseEdge :: String -> Edge
parseEdge line =
	let	fields = splitOn "\t" line
	in Edge (fields!!1) (fields!!2)

unparsePlacements :: Maybe Placements -> String
unparsePlacements Nothing = "No placements"
unparsePlacements (Just placements) = unlines $ map (\(Placement (Point x y) node) -> ("[" ++ node ++ " (" ++ (show x) ++ ", " ++ (show y) ++ ")]")) placements
--unparsePlacements (Just placements) = foldr (\placement acc -> (show placement) ++ "," ++ acc) "" placements

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
