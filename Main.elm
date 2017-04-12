module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra
import Time exposing (Time, millisecond)
import Array
-- import Debug


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


type alias Path =
    List Point


type alias Model =
    { arena : Arena
    , character : Character
    }



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
    , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Gr 1, Gr 2, Gr 2, Gr 1, Wall, Gr 1, Wall ]
    , [ Wall, Wall, Gr 1, Gr 4, Gr 1, Wall, Gr 1, Wall ]
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
pathFind originPoint target arena =
    let
        origin = PathNode originPoint Empty 0
        closedSet = []
        openSet = [ origin ]

        exploredNodes =
            exploreNodes openSet closedSet target arena

        targetNode =
            exploredNodes
                |> List.filter (\node -> target == node.location)
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

        Predecessor predNode ->
            tracePathBack predNode exploredNodes (node.location :: currentPath)


exploreNodes : List PathNode -> List PathNode -> Point -> Arena -> List PathNode
exploreNodes openSet closedSet target arena =
    let
        origin = lowestCostNode openSet

        neighbors =
            (nodeNeighbors origin arena)
                |> List.filterMap (lowerCostNode closedSet)

        newOpenSet =
            openSet
                |> List.filter (\node -> origin.location /= node.location)
                |> (++) neighbors

        newClosedSet =
            origin :: closedSet

        foundTarget =
            newClosedSet
                |> List.filter (\node -> target == node.location)
                |> List.head

    in
        case foundTarget of
            Nothing ->
                if List.length openSet > 0 then
                    exploreNodes newOpenSet newClosedSet target arena
                else
                    newClosedSet
            Just _ ->
                newClosedSet

lowestCostNode : List PathNode -> PathNode
lowestCostNode set =
    List.sortBy .cost set
        |> List.head
        |> Maybe.withDefault { location = pointOrigin, cameFrom = Empty, cost = 9999 }


locationsEqual : Point -> Point -> Bool
locationsEqual a b =
    (a.x == b.x) && (a.y == b.y)



nodeNeighbors : PathNode -> Arena -> List PathNode
nodeNeighbors node arena =
    let
        grid =
            List.Extra.lift2 (,) [ -1, 0, 1 ] [ -1, 0, 1 ]
                |> List.filter (\( x, y ) -> x /= 0 || y /= 0)
                |> List.map (\( x, y ) -> Point x y)
    in
        List.filterMap (checkNode node arena) grid


checkNode : PathNode -> Arena -> Point -> Maybe PathNode
checkNode node arena offset =
    let
        actualLocation =
            { x = node.location.x + offset.x
            , y = node.location.y + offset.y
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
    Time.every (1000 * millisecond) Advance



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
    [ ( "width", "80px" )
    , ( "height", "80px" )
    , ( "margin", "10px" )
    , ( "position", "absolute" )
    , ( "left", positionInPx char.location.x )
    , ( "top", positionInPx char.location.y )
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
    , ( "left", positionInPx char.target.x )
    , ( "top", positionInPx char.target.y )
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


pointOrigin : Point
pointOrigin =
    Point 0 0
