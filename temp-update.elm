-- Code this
    let
        closedSet =
            []

        openSet =
            [ PathNode origin Empty 0 ]

        exploredNodes =
            exploreNodes openSet closedSet arena

        foundTarget =
            exploredNodes
                |> List.filter (\node -> locationsEqual node.location target)
                |> List.head
    in
        case foundTarget of
            Nothing ->
                []

            Just pathNode ->
                tracePathBack pathNode exploredNodes []
