module Main exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
-- import Html.Events exposing (onClick)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Msg = None

type Terrain
  = Wall
  | Gr Float

type alias Arena = List (List Terrain)

type alias Character =
  { x : Int
  , y : Int
  }

type alias Model =
  { arena : Arena
  , character : Character
  }

-- 8x8 arena map
initArena : Arena
initArena =
  [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Wall ]
  , [ Wall, Wall, Wall, Gr 1, Gr 1, Wall, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 1, Wall, Wall, Wall, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
  , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
  ]

initCharacter : Character
initCharacter =
  { x = 1, y = 1 }

init : ( Model, Cmd Msg )
init =
  ({ arena = initArena
   , character = initCharacter
   }, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

containerStyle : List (String, String)
containerStyle =
  [ ("margin", "40px auto"), ("width", "800px"), ("height", "800px"), ("backgroundColor", "gray"), ("position", "relative") ]

blockStyle : String -> List (String, String)
blockStyle colorVal =
  [ ("width", "100px"), ("height", "100px"), ("backgroundColor", colorVal), ("float", "left"), ("outline", "1px solid darkgray") ]

terrainColor : Terrain -> String
terrainColor terr =
  case terr of
    Wall -> "dimgray"
    Gr n -> "lightgray"

characterStyle : Character -> List (String, String)
characterStyle char =
  [ ("width", "80px")
  , ("height", "80px")
  , ("margin", "10px")
  , ("position", "absolute")
  , ("left", (toString (char.x * 100)) ++ "px")
  , ("top", (toString (char.y * 100)) ++ "px")
  , ("backgroundColor", "lightsalmon")
  , ("borderRadius", "40px")
  , ("boxShadow", "2px 2px 4px 1px salmon")
  ]

view : Model -> Html.Html Msg
view model =
  div [ style containerStyle ] ((arenaView model.arena) ++ (characterView model.character))

arenaView : Arena -> List (Html msg)
arenaView arena =
  List.concatMap arenaRowView arena

arenaRowView : List Terrain -> List (Html msg)
arenaRowView row =
  List.map arenaBlockView row

arenaBlockView : Terrain -> Html msg
arenaBlockView terr =
  div [ style (blockStyle (terrainColor terr)) ] []

characterView : Character -> List (Html msg)
characterView char =
  [ (div [ style (characterStyle char) ] []) ]
