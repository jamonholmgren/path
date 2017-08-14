-- Copy & explain these ones

nodeNeighbors : PathNode -> Arena -> List PathNode
nodeNeighbors node arena =
    let
        grid =
            [ Point -1 -1,  Point 0 -1, Point 1 -1
            , Point -1 0,               Point 1 0
            , Point -1 1,   Point 0 1,  Point 1 1
            ]
    in
        List.filterMap (checkNode node arena) grid


checkNode : PathNode -> Arena -> Point -> Maybe PathNode
checkNode node arena point =
    let
        actualLocation =
            { x = node.location.x + point.x
            , y = node.location.y + point.y
            }

        actualTerrain =
            arenaTerrain arena actualLocation
    in
        case actualTerrain of
            Nothing ->
                Nothing

            Just Wall ->
                Nothing

            Just (Gr c) ->
                let
                    cost =
                        if actualLocation.x /= 0 && actualLocation.y /= 0 then
                            c * (sqrt 2)
                        else
                            c
                in
                    Just
                        { location = actualLocation
                        , cameFrom = Predecessor node
                        , cost =
                            node.cost + cost
                        }
