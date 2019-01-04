module Main exposing (Arena, Character, Model, Msg(..), Path, PathNode, PathTree(..), Point, Terrain(..), arenaBlockView, arenaRowView, arenaTerrain, arenaView, blockLabel, blockStyle, characterStyle, characterView, checkNode, containerStyle, exploreNodes, init, initArena, initCharacter, locationsEqual, lowerCostNode, main, nodeNeighbors, pathFind, pointOrigin, positionInPx, sign, subscriptions, targetStyle, terrainColor, tracePathBack, update, view)

import Array
import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = Advance Time.Posix
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


pointOrigin : Point
pointOrigin =
    Point 0 0


type alias Character =
    { location : Point
    , target : Point
    , targetPath : Path
    }


type alias Path =
    List Point


type alias Model =
    { arena : Arena
    , character : Character
    }



-- Main init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { arena = initArena
      , character = initCharacter
      }
    , Cmd.none
    )



-- 8x8 arena map


initArena : Arena
initArena =
    [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 2, Gr 2, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Wall, Wall, Gr 4, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Gr 4, Gr 6, Wall, Wall, Wall, Gr 8, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
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
                                | location =
                                    { x = character.location.x + sign (step.x - character.location.x)
                                    , y = character.location.y + sign (step.y - character.location.y)
                                    }
                                , targetPath = remainingPath
                            }
            in
            ( { model | character = char }, Cmd.none )

        SetTarget x y ->
            let
                newPath =
                    pathFind character.location { x = x, y = y } arena

                newTarget =
                    { x = x, y = y }
            in
            ( { model | character = { character | target = newTarget, targetPath = newPath } }, Cmd.none )


type PathTree
    = Empty
    | Predecessor PathNode


type alias PathNode =
    { location : Point
    , cameFrom : PathTree
    , cost : Float
    }


pathFind : Point -> Point -> Arena -> Path
pathFind character target arena =
    let
        closedSet =
            []

        openSet =
            [ PathNode character Empty 0 ]

        exploredNodes =
            exploreNodes openSet closedSet arena

        targetNode =
            exploredNodes
                |> List.filter (\node -> locationsEqual target node.location)
                |> List.head
    in
    case targetNode of
        Nothing ->
            []

        Just pathNode ->
            tracePathBack pathNode exploredNodes []


tracePathBack : PathNode -> List PathNode -> Path -> Path
tracePathBack node exploredNodes currentPath =
    case node.cameFrom of
        Empty ->
            currentPath

        Predecessor pred ->
            let
                prevLocation =
                    { x = node.location.x, y = node.location.y }
            in
            tracePathBack pred exploredNodes (prevLocation :: currentPath)


locationsEqual : Point -> Point -> Bool
locationsEqual a b =
    (a.x == b.x) && (a.y == b.y)


exploreNodes : List PathNode -> List PathNode -> Arena -> List PathNode
exploreNodes openSet closedSet arena =
    let
        origin =
            List.sortBy .cost openSet
                |> List.head
                |> Maybe.withDefault { location = pointOrigin, cameFrom = Empty, cost = 9999 }

        neighbors =
            nodeNeighbors origin arena
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


lowerCostNode : List PathNode -> PathNode -> Maybe PathNode
lowerCostNode closedSet neighborNode =
    let
        matchingNode =
            closedSet
                |> List.filter (\node -> locationsEqual neighborNode.location node.location)
                |> List.head
    in
    case matchingNode of
        Nothing ->
            Just neighborNode

        Just n ->
            if n.cost > neighborNode.cost then
                Just neighborNode

            else
                Nothing


nodeNeighbors : PathNode -> Arena -> List PathNode
nodeNeighbors node arena =
    let
        grid =
            List.Extra.lift2 (\a b -> ( a, b )) [ -1, 0, 1 ] [ -1, 0, 1 ]
                |> List.filter (\( x, y ) -> x /= 0 || y /= 0)
                |> List.map (\( x, y ) -> Point x y)
    in
    List.filterMap (checkNode node arena) grid


checkNode : PathNode -> Arena -> Point -> Maybe PathNode
checkNode node arena point =
    let
        actualLocation =
            { x = node.location.x + point.x
            , y = node.location.y + point.y
            }

    in
    arenaTerrain arena actualLocation
        |> Maybe.andThen (terrainToCost actualLocation)
        |> Maybe.map
            (\cost ->
                { location = actualLocation
                , cameFrom = Predecessor node
                , cost =
                    node.cost + cost
                }
            )


terrainToCost : Point -> Terrain -> Maybe Float
terrainToCost location terrain =
    case terrain of
        Wall ->
            Nothing

        Gr c ->
            let
                cost =
                    if location.x /= 0 && location.y /= 0 then
                        c * sqrt 2

                    else
                        c
            in
            Just cost


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
subscriptions _ =
    Time.every 1000 Advance



-- Main view


view : Model -> Html.Html Msg
view model =
    div containerStyle (arenaView model.arena ++ characterView model.character)



-- View components


arenaView : Arena -> List (Html Msg)
arenaView rows =
    List.concat (List.indexedMap arenaRowView rows)


arenaRowView : Int -> List Terrain -> List (Html Msg)
arenaRowView index row =
    List.indexedMap (arenaBlockView index) row


arenaBlockView : Int -> Int -> Terrain -> Html Msg
arenaBlockView row col terr =
    div (blockStyle (terrainColor terr) ++ [ onClick (SetTarget col row) ]) [ text (blockLabel row col terr) ]


characterView : Character -> List (Html Msg)
characterView char =
    [ div (characterStyle char) []
    , div (targetStyle char) []
    ]



-- View styles


blockLabel : Int -> Int -> Terrain -> String
blockLabel row col terr =
    case terr of
        Wall ->
            ""

        Gr cost ->
            String.fromInt col
                ++ "•"
                ++ String.fromInt row
                ++ " ("
                ++ String.fromFloat cost
                ++ ")"


containerStyle : List (Html.Attribute msg)
containerStyle =
    [ style "margin" "40px auto"
    , style "width" "800px"
    , style "height" "800px"
    , style "backgroundColor" "gray"
    , style "position" "relative"
    , style "fontFamily" "sans-serif"
    , style "color" "black"
    , style "fontSize" "8px"
    ]


blockStyle : String -> List (Html.Attribute msg)
blockStyle colorVal =
    [ style "width" "100px"
    , style "height" "100px"
    , style "backgroundColor" colorVal
    , style "float" "left"
    , style "outline" "1px solid slategray"
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
                |> List.map String.fromFloat
                |> String.join ", "
                |> (\values -> "rgb(" ++ values ++ ")")


characterStyle : Character -> List (Html.Attribute msg)
characterStyle char =
    [ style "width" "80px"
    , style "height" "80px"
    , style "margin" "10px"
    , style "position" "absolute"
    , style "left" (positionInPx char.location.x)
    , style "top" (positionInPx char.location.y)
    , style "backgroundColor" "lightsalmon"
    , style "borderRadius" "40px"
    , style "boxShadow" "2px 2px 4px 1px salmon"
    , style "transition" "top 1s, left 1s"
    ]


targetStyle : Character -> List (Html.Attribute msg)
targetStyle char =
    [ style "width" "20px"
    , style "height" "20px"
    , style "margin" "40px"
    , style "position" "absolute"
    , style "left" (positionInPx char.target.x)
    , style "top" (positionInPx char.target.y)
    , style "backgroundColor" "lightcoral"
    , style "borderRadius" "3px"
    , style "boxShadow" "2px 2px 4px 1px gray"
    ]



-- Utility functions


sign : Int -> Int
sign num =
    case num of
        0 ->
            0

        n ->
            round (toFloat n / abs (toFloat n))


positionInPx : Int -> String
positionInPx position =
    String.fromInt (position * 100) ++ "px"
