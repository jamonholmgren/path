module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time exposing (Time, millisecond)
import Array


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


type alias Character =
    { x : Int
    , y : Int
    , targetX : Int
    , targetY : Int
    , targetPath : Path
    }


type alias Model =
    { arena : Arena
    , character : Character
    }


type alias Path =
    List { x : Int, y : Int }



-- Main init


init : ( Model, Cmd Msg )
init =
    ( { arena = initArena
      , character = initCharacter
      }
    , Cmd.none
    )



-- 8x8 arena map


initArena : Arena
initArena =
    [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
    , [ Wall, Gr 1, Gr 2, Gr 1, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 2, Gr 2, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Wall, Wall, Gr 2, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 2, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Wall, Wall, Wall, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
    , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
    ]


initCharacter : Character
initCharacter =
    { x = 1
    , y = 2
    , targetX = 1
    , targetY = 2
    , targetPath = []
    }



-- Main update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ character, arena } as model) =
    case msg of
        Advance _ ->
            let
                -- TODO: Implement path-following algorithm
                char =
                    case character.targetPath of
                        [] ->
                            character

                        step :: remainingPath ->
                            { character
                                | x = (character.x + (sign (step.x - character.x)))
                                , y = (character.y + (sign (step.y - character.y)))
                                , targetPath = remainingPath
                            }
            in
                ( { model | character = char }, Cmd.none )

        SetTarget x y ->
            let
                newPath =
                    pathFind character.x character.y x y arena

                -- TODO
            in
                ( { model | character = { character | targetX = x, targetY = y, targetPath = newPath } }, Cmd.none )



-- Main subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (1000 * millisecond) Advance
        ]



-- Main view


view : Model -> Html.Html Msg
view model =
    div [ style containerStyle ] ((arenaView model.arena) ++ (characterView model.character))



-- View components


arenaView : Arena -> List (Html Msg)
arenaView rows =
    List.concat (List.indexedMap arenaRowView rows)


arenaRowView : Int -> List Terrain -> List (Html Msg)
arenaRowView index row =
    List.indexedMap (arenaBlockView index) row


arenaBlockView : Int -> Int -> Terrain -> Html Msg
arenaBlockView row col terr =
    div [ style (blockStyle (terrainColor terr)), (onClick (SetTarget col row)) ] [ text (blockLabel row col terr) ]


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
    [ ( "width", "100px" )
    , ( "height", "100px" )
    , ( "backgroundColor", colorVal )
    , ( "float", "left" )
    , ( "outline", "1px solid slategray" )
    ]


terrainColor : Terrain -> String
terrainColor terr =
    case terr of
        Wall ->
            "dimgray"

        Gr 1 ->
            "lightgray"

        Gr n ->
            "darkgray"


characterStyle : Character -> List ( String, String )
characterStyle char =
    [ ( "width", "80px" )
    , ( "height", "80px" )
    , ( "margin", "10px" )
    , ( "position", "absolute" )
    , ( "left", positionInPx char.x )
    , ( "top", positionInPx char.y )
    , ( "backgroundColor", "lightsalmon" )
    , ( "borderRadius", "40px" )
    , ( "boxShadow", "2px 2px 4px 1px salmon" )
    , ( "transition", "top 1s, left 1s" )
    ]


targetStyle : Character -> List ( String, String )
targetStyle char =
    [ ( "width", "20px" )
    , ( "height", "20px" )
    , ( "margin", "40px" )
    , ( "position", "absolute" )
    , ( "left", positionInPx char.targetX )
    , ( "top", positionInPx char.targetY )
    , ( "backgroundColor", "lightcoral" )
    , ( "borderRadius", "3px" )
    , ( "boxShadow", "2px 2px 4px 1px gray" )
    ]



-- Utility functions


sign : Int -> Int
sign num =
    case num of
        0 ->
            0

        n ->
            round (toFloat (n) / abs (toFloat (n)))


positionInPx : number -> String
positionInPx position =
    toString (position * 100) ++ "px"


type PathNode
    = PathNode
        { x : Int
        , y : Int
        , cameFrom : Maybe PathNode
        , cost : Float
        }


emptyPathNode : PathNode
emptyPathNode =
    PathNode { x = 0, y = 0, cameFrom = Nothing, cost = 9999 }


pathFind : Int -> Int -> Int -> Int -> Arena -> Path
pathFind cx cy x y arena =
    let
        closedSet =
            []

        openSet =
            [ PathNode { x = cx, y = cy, cameFrom = Nothing, cost = 0 } ]

        finalPath =
            exploreNodes openSet closedSet arena

        -- activeNodes =
        --     exploreNode x y exploredNodes arena origin
    in
        []


lowestCostNode : List PathNode -> PathNode -> PathNode
lowestCostNode closedSet node =
    let
        matchingNode =
            closedSet
                |> List.filter (\n -> (n.x == (getPathNodeX node)) && (n.y == (getPathNodeY node)))
                |> List.head
    in
        case matchingNode of
            Nothing ->
                node

            PathNode n ->
                if n.cost > (getPathNode node).cost then
                    Just n
                else
                    node


exploreNodes : List PathNode -> List PathNode -> Arena -> Path
exploreNodes openSet closedSet arena =
    let
        origin =
            List.sortBy .cost openSet
                |> List.head
                |> Maybe.withDefault emptyPathNode

        neighbors =
            (nodeNeighbors origin arena)
                |> List.filterMap (lowestCostNode closedSet)

        -- neighbors - filter out the ones that aren't shorter
        -- remove me from currently exploring list
        -- add me to already explored list
        -- add neighbors to currently exploring list
        -- select the smallest cost in currently exploring list
        -- get it's neighbors & repeat
        -- activeNeighbors =
        --     List.filter (shortestNeighbor exploredNodes)
        --
        -- newExploredNodes =
        --     exploredNodes ++ neighbors
        -- which neighbors are already in exploredNodes
        -- compare the new cost (neighbor) to already explored in exploredNodes
    in
        []


nodeNeighbors : PathNode -> Arena -> List PathNode
nodeNeighbors node arena =
    List.filterMap (checkNode node arena) (List.map2 (,) [ -1, 0, 1 ] [ -1, 0, 1 ])


checkNode : PathNode -> Arena -> ( Int, Int ) -> Maybe PathNode
checkNode node arena offset =
    case offset of
        ( 0, 0 ) ->
            Nothing

        ( x, y ) ->
            let
                actualX =
                    (getPathNodeX node) + x

                actualY =
                    (getPathNodeY node) + y

                actualTerrain =
                    arenaTerrain arena actualX actualY
            in
                case actualTerrain of
                    Nothing ->
                        Nothing

                    Just Wall ->
                        Nothing

                    Just (Gr n) ->
                        Just
                            (PathNode
                                { x = actualX
                                , y = actualY
                                , cameFrom = Nothing
                                , cost = n
                                }
                            )


arenaTerrain : Arena -> Int -> Int -> Maybe Terrain
arenaTerrain arena x y =
    let
        arenaArray =
            Array.fromList (List.map Array.fromList arena)

        row =
            Array.get y arenaArray
    in
        Maybe.andThen (Array.get x) row


getPathNode : PathNode -> { x : Int, y : Int, cameFrom : Maybe PathNode, cost : Float }
getPathNode ((PathNode { x, y, cost, cameFrom }) as node) =
    { x = x, y = y, cost = cost, cameFrom = cameFrom }


getPathNodeX : PathNode -> Int
getPathNodeX (PathNode { x }) =
    x


getPathNodeY : PathNode -> Int
getPathNodeY (PathNode { y }) =
    y
