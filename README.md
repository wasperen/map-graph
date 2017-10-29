= Map Graph =
This is an experiment to
 1. learn Haskell
 2. place all nodes in a Graph nicely onto a 2-dimensional grid

Of course I first tried a "force field simulation" but was not happy with the resulting layout.
Force fields are great, interactively, to show and explore a graph. But I am looking for a more
rigid and stable layout that just shows my graph in an orderly fashion.

And I am not shying away from "brute force". The Haskell implementation that I am now working on
works from the following principle:

 1. read the edges
 2. find the "starting nodes" (the ones that have no predecessor)
 3. place the starting nodes on the grid
 4. for each edge of the non-starter kind
   a. find the possible placings, close to the node that precedes it in the graph
   b. if we can place it at the first placing, go for it and try to place the remainder
   c. if the remainder fails, try the next possible placing
   d. if all possible placings fail, fail to place this node
 5. if we placed all nodes, print out the placements, else "No Placements"

This should basically try out all possible layouts untill it finds a suitable one.

Some functions that I am now working on:

 * the possible placings for a node are all points directly arround the preceding node
 * we start exploring new nodes to the East, North, West or South, depending on what quadrant we are in
 * we rotate these possible placings clockwize for x >= 0 and anticlockwize for x < 0

 * the starting positions are points around (0,0), starting at a certain distance and increasing from there
