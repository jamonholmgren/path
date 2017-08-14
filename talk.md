# A* Pathfinding Algorithm

by Jamon Holmgren, COO/CTO @ Infinite Red, Inc.
@jamonholmgren (Twitter & Github)

Source: https://github.com/jamonholmgren/path
`pdxfunc-meetup-2017` branch

* Invented in 1968 (based on Dijkstra's Algorithm)
* Used in games to find most efficient (not necessarily shortest) path
* Many different variants

## Basics

1. Constructs a tree of _nodes_ from the origin to the target
2. Calculates costs to get to each node, resolves intersecting paths
3. Open set = "fringe" (the outermost expanding line of explored nodes)
4. Closed set = "interior" (nodes already fully explored)

Show Astar_progress_animation.gif
