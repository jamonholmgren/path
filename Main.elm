module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time exposing (Time, millisecond)
import Array
import Debug


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = Advance Time
    | SetTarget Int Int


type Terrain
    = Wall
    | Gr Float



-- Ground & movement cost points


type alias Arena =
    List (List Terrain)


type alias Point =
    { x : Int
    , y : Int
    }


type alias Character =
    { location : Point
    , target : Point
    , targetPath : Path
    }


-- The (eventual) final path to follow
type alias Path =
    List Point


-- Required to avoid circular type alias definition PathNode
type PathTree
    = Empty
    | Predecessor PathNode


-- This is what we will be using to explore the map
type alias PathNode =
    { location : Point
    , cameFrom : PathTree
    , cost : Float
    }


type alias Model =
    { arena : Arena
    , character : Character
    , closedSet : List PathNode
    }



-- Main init


init : ( Model, Cmd Msg )
init =
    ( { arena = initArena
      , character = initCharacter
      , closedSet = []
      }
    , Cmd.none
    )



-- 8x8 arena map


initArena : Arena
initArena =
    [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 2, Gr 2, Gr 1, Wall, Gr 1, Gr 1, Wall, Gr 1, Wall, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Wall, Wall, Gr 4, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Wall, Gr 1, Wall, Wall, Wall, Wall, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Wall, Wall, Wall, Gr 1, Wall, Gr 1, Wall, Gr 1, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Gr 4, Gr 6, Wall, Wall, Wall, Gr 8, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Wall, Gr 1, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Wall, Wall, Wall, Wall, Gr 1, Wall, Wall, Gr 3, Wall, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Wall, Wall, Wall, Gr 1, Wall, Gr 1, Wall, Wall, Wall, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Wall, Wall, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 9, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Wall, Wall, Wall, Gr 1, Wall, Gr 1, Gr 9, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Wall, Wall, Wall, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 9, Gr 1, Gr 1, Gr 1, Gr 3, Gr 5, Wall, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Wall, Gr 5, Gr 3, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Wall, Gr 1, Wall, Gr 1, Wall, Wall, Wall, Gr 1, Wall, Wall, Wall, Wall, Wall, Gr 3, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Wall, Gr 1, Wall, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Gr 3, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Wall, Gr 1, Wall, Gr 1, Wall, Gr 1, Wall, Gr 1, Gr 9, Gr 2, Gr 2, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Gr 9, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
    ]


initCharacter : Character
initCharacter =
    { location = Point 1 2, target = Point 1 2, targetPath = [] }



-- Main update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ character, arena } as model) =
    case msg of
        Advance _ ->
            let
                char =
                    case character.targetPath of
                        [] ->
                            character

                        step :: remainingPath ->
                            { character
                                | location = (updateLocation character.location step)
                                , targetPath = remainingPath
                            }
            in
                ( { model | character = char }, Cmd.none )

        SetTarget x y ->
            let
                newTarget =
                    Point x y

                (newPath, newClosedSet) =
                    pathFind character.location newTarget arena

                newCharacter =
                    { character | target = newTarget, targetPath = newPath }

            in
                ( { model | character = newCharacter, closedSet = newClosedSet }, Cmd.none )


updateLocation : Point -> Point -> Point
updateLocation location step =
    { x = location.x + sign (step.x - location.x)
    , y = location.y + sign (step.y - location.y)
    }


-- Where we will spend most of our time ---------------------------------

pathFind : Point -> Point -> Arena -> ( Path, List PathNode )
pathFind origin target arena =
    let
        openSet =
            [ PathNode origin Empty 0 ]

        closedSet =
            exploreNodes openSet [] arena target

        foundTarget =
            closedSet
            |> List.filter (\node -> locationsEqual target node.location)
            |> List.head
    in
        case foundTarget of
            Nothing ->
                ([], closedSet)

            Just pathNode ->
                (tracePathBack pathNode [], closedSet)


tracePathBack : PathNode -> Path -> Path
tracePathBack pathNode currentPath =
    case pathNode.cameFrom of
        Empty ->
            currentPath

        Predecessor pred ->
            tracePathBack pred (pathNode.location :: currentPath)


exploreNodes : List PathNode -> List PathNode -> Arena -> Point -> List PathNode
exploreNodes openSet closedSet arena target =
    let
        origin =
            openSet
            |> List.sortBy .cost
            |> List.head
            |> Maybe.withDefault (PathNode (Point 0 0) Empty 0)

        neighbors =
            nodeNeighbors origin arena target
            |> List.filter (lowerCostNode closedSet)

        foo = Debug.log "Neighbors" (toString neighbors)

        newOpenSet =
            openSet
            |> List.filter (\node -> not (locationsEqual origin.location node.location))
            |> (++) neighbors

        newClosedSet =
            origin :: closedSet

        foundTarget =
            newClosedSet
            |> List.filter (\node -> locationsEqual node.location target)
            |> List.head
    in
        case foundTarget of
            Nothing ->
                if List.length newOpenSet > 0 then
                    exploreNodes newOpenSet newClosedSet arena target
                else
                    newClosedSet
            Just targetNode ->
                newClosedSet


lowerCostNode : List PathNode -> PathNode -> Bool
lowerCostNode closedSet neighborNode =
    closedSet
    |> List.filter(\closedNode -> locationsEqual closedNode.location neighborNode.location)
    |> List.isEmpty

-- Uncomment and explain nodeNeighbors and checkNode

nodeNeighbors : PathNode -> Arena -> Point -> List PathNode
nodeNeighbors node arena target =
    let
        grid =
            [ Point -1 -1,  Point 0 -1, Point 1 -1
            , Point -1 0,               Point 1 0
            , Point -1 1,   Point 0 1,  Point 1 1
            ]
    in
        List.filterMap (checkNode node arena target) grid


checkNode : PathNode -> Arena -> Point -> Point -> Maybe PathNode
checkNode node arena target point =
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
                    terrainFactor = 1.0 -- how much terrain affects heuristic
                    distanceFactor = 10.0 -- how much distance affects heuristic
                    
                    distX = toFloat (abs (actualLocation.x - target.x))
                    distY = toFloat (abs (actualLocation.y - target.y))

                    distanceToTarget =
                        sqrt (distX * distX + distY * distY) * distanceFactor

                    cost =
                        if point.x /= 0 && point.y /= 0 then
                            (c * terrainFactor) * (sqrt 2) + distanceToTarget
                        else
                            (c * terrainFactor) + distanceToTarget
                in
                    Just
                        { location = actualLocation
                        , cameFrom = Predecessor node
                        , cost =
                            node.cost + cost
                        }


-- end --------------------------------- --------------------------------

arenaTerrain : Arena -> Point -> Maybe Terrain
arenaTerrain arena point =
    let
        arenaArray =
            Array.fromList (List.map Array.fromList arena)

        row =
            Array.get point.y arenaArray
    in
        Maybe.andThen (Array.get point.x) row



-- Main subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (250 * millisecond) Advance
        ]


-- Main view


view : Model -> Html.Html Msg
view model =
    div [ style containerStyle ] ((arenaView model.arena model.closedSet) ++ (characterView model.character))


-- View components


arenaView : Arena -> List PathNode -> List (Html Msg)
arenaView rows closedSet =
    List.concat (List.indexedMap (arenaRowView closedSet) rows)


arenaRowView : List PathNode -> Int -> List Terrain -> List (Html Msg)
arenaRowView closedSet index row =
    List.indexedMap (arenaBlockView closedSet index) row


arenaBlockView : List PathNode -> Int -> Int -> Terrain -> Html Msg
arenaBlockView closedSet row col terr =
    let
        matchedNode =
            closedSet
            |> List.filter (\node -> node.location.x == col && node.location.y == row)
            |> List.head

        longestPathNode =
            closedSet
            |> List.sortBy .cost
            |> List.reverse
            |> List.head
            |> Maybe.withDefault (PathNode (Point 0 0) Empty 255)
        
        blockColor =
            case matchedNode of
                Nothing ->
                    terrainColor terr

                Just node ->
                    "rgb(10, " ++ (toString (255 - (round ((node.cost / longestPathNode.cost) * 240)))) ++ ", 10)"
    in
        div [ style (blockStyle blockColor), (onClick (SetTarget col row)) ] [ text (blockLabel row col terr) ]


characterView : Character -> List (Html Msg)
characterView char =
    [ div [ style (characterStyle char) ] []
    , div [ style (targetStyle char) ] []
    ]


-- View styles


blockLabel : Int -> Int -> Terrain -> String
blockLabel row col terr =
    case terr of
        Wall ->
            ""

        Gr cost ->
            (toString col)
                ++ "•"
                ++ (toString row)
                ++ " ("
                ++ (toString cost)
                ++ ")"


containerStyle : List ( String, String )
containerStyle =
    [ ( "margin", "40px auto" )
    , ( "width", "800px" )
    , ( "height", "800px" )
    , ( "backgroundColor", "gray" )
    , ( "position", "relative" )
    , ( "fontFamily", "sans-serif" )
    , ( "color", "black" )
    , ( "fontSize", "8px" )
    ]


blockStyle : String -> List ( String, String )
blockStyle colorVal =
    [ ( "width", "40px" )
    , ( "height", "40px" )
    , ( "backgroundColor", colorVal )
    , ( "float", "left" )
    , ( "outline", "1px solid slategray" )
    ]


terrainColor : Terrain -> String
terrainColor terr =
    case terr of
        Wall ->
            "rgb(50, 50, 50)"

        Gr n ->
            let
                base =
                    250

                offset =
                    20
            in
                List.repeat 3 (base - n * offset)
                    |> List.map toString
                    |> String.join ", "
                    |> \values -> "rgb(" ++ values ++ ")"


characterStyle : Character -> List ( String, String )
characterStyle char =
    [ ( "width", "25px" )
    , ( "height", "25px" )
    , ( "margin", "5px" )
    , ( "position", "absolute" )
    , ( "left", positionInPx char.location.x )
    , ( "top", positionInPx char.location.y )
    , ( "backgroundColor", "lightsalmon" )
    , ( "borderRadius", "20px" )
    , ( "boxShadow", "2px 2px 4px 1px salmon" )
    , ( "transition", "top 0.25s, left 0.25s" )
    ]


targetStyle : Character -> List ( String, String )
targetStyle char =
    [ ( "width", "10px" )
    , ( "height", "10px" )
    , ( "margin", "10px" )
    , ( "position", "absolute" )
    , ( "left", positionInPx char.target.x )
    , ( "top", positionInPx char.target.y )
    , ( "backgroundColor", "lightcoral" )
    , ( "borderRadius", "3px" )
    , ( "boxShadow", "2px 2px 4px 1px gray" )
    ]



-- Utility functions


locationsEqual : Point -> Point -> Bool
locationsEqual a b =
    (a.x == b.x) && (a.y == b.y)


sign : Int -> Int
sign num =
    case num of
        0 ->
            0

        n ->
            round (toFloat (n) / abs (toFloat (n)))

positionInPx : number -> String
positionInPx position =
    toString (position * 40) ++ "px"


pointOrigin : Point
pointOrigin =
    Point 0 0
