-- Code this one

tracePathBack : PathNode -> List PathNode -> Path -> Path
tracePathBack pathNode exploredNodes currentPath =
    case pathNode.cameFrom of
        Empty ->
            currentPath

        Predecessor predecessor ->
            tracePathBack predecessor exploredNodes (pathNode.location :: currentPath)
