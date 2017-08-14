-- Code this, maybe copy lowerCostNode below

exploreNodes : List PathNode -> List PathNode -> Arena -> List PathNode
exploreNodes openSet closedSet arena =
    let
        origin =
            List.sortBy .cost openSet
                |> List.head
                |> Maybe.withDefault { location = pointOrigin, cameFrom = Empty, cost = 9999 }

        neighbors =
            (nodeNeighbors origin arena)
                |> List.filterMap (lowerCostNode closedSet)

        newOpenSet =
            openSet
                |> List.filter (\node -> not (locationsEqual origin.location node.location))
                |> (++) neighbors

        newClosedSet =
            origin :: closedSet
    in
        if List.length openSet > 0 then
            exploreNodes newOpenSet newClosedSet arena
        else
            newClosedSet

-- Either code this or copy it

lowerCostNode : List PathNode -> PathNode -> Bool
lowerCostNode closedSet neighborNode =
    closedSet
    |> List.filter(\closedNode -> locationsEqual closedNode.location neighborNode.location)
    |> List.isEmpty
    --
    -- let
    --     matchingNode =
    --         closedSet
    --             |> List.filter (\node -> locationsEqual neighborNode.location node.location)
    --             |> List.head
    -- in
    --     case matchingNode of
    --         Nothing ->
    --             Just neighborNode
    --
    --         Just n ->
    --             if n.cost > neighborNode.cost then
    --                 Just neighborNode
    --             else
    --                 Nothing
