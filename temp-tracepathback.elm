-- Code this one

tracePathBack : PathNode -> Path -> Path
tracePathBack pathNode currentPath =
    case pathNode.cameFrom of
        Empty ->
            currentPath

        Predecessor predecessor ->
            tracePathBack predecessor (pathNode.location :: currentPath)
