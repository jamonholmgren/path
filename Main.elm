module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time exposing (Time, millisecond)

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
  | Gr Float -- Ground & movement cost points

type alias Arena = List (List Terrain)

type alias Character =
  { x : Int
  , y : Int
  , targetX : Int
  , targetY : Int
  }

type alias Model =
  { arena : Arena
  , character : Character
  }

-- Main init

init : ( Model, Cmd Msg )
init =
  ({ arena = initArena
   , character = initCharacter
   }, Cmd.none)

-- 8x8 arena map
initArena : Arena
initArena =
  [ [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Wall, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 2, Gr 2, Gr 1, Wall, Gr 1, Wall ]
  , [ Wall, Wall, Wall, Gr 2, Gr 1, Wall, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 2, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 1, Wall, Wall, Wall, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Gr 1, Wall ]
  , [ Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ]
  ]

initCharacter : Character
initCharacter =
  { x = 1, y = 2, targetX = 6, targetY = 1 }

-- Main update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({character} as model) =
  case msg of
    Advance _ ->
      let
        char =
          { character
          | x = (character.x + (sign (character.targetX - character.x)))
          , y = (character.y + (sign (character.targetY - character.y)))
          }
      in
        ({ model | character = char }, Cmd.none)
    SetTarget row col ->
      ({ model | character = { character | targetX = col, targetY = row } }, Cmd.none)

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
  div [ style (blockStyle (terrainColor terr)), (onClick (SetTarget row col)) ] [ text (blockLabel row col terr) ]

characterView : Character -> List (Html Msg)
characterView char =
  [ div [ style (characterStyle char) ] []
  , div [ style (targetStyle char) ] []
  ]

-- View styles

blockLabel : Int -> Int -> Terrain -> String
blockLabel row col terr =
  case terr of
    Wall -> ""
    Gr cost ->
      (toString col)
      ++ "•"
      ++ (toString row)
      ++ " ("
      ++ (toString cost)
      ++ ")"

containerStyle : List (String, String)
containerStyle =
  [ ("margin", "40px auto")
  , ("width", "800px")
  , ("height", "800px")
  , ("backgroundColor", "gray")
  , ("position", "relative")
  , ("fontFamily", "sans-serif")
  , ("color", "black")
  , ("fontSize", "8px")
  ]

blockStyle : String -> List (String, String)
blockStyle colorVal =
  [ ("width", "100px")
  , ("height", "100px")
  , ("backgroundColor", colorVal)
  , ("float", "left")
  , ("outline", "1px solid slategray")
  ]

terrainColor : Terrain -> String
terrainColor terr =
  case terr of
    Wall -> "dimgray"
    Gr 1 -> "lightgray"
    Gr n -> "darkgray"

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
  , ("transition", "top 1s, left 1s")
  ]

targetStyle : Character -> List (String, String)
targetStyle char =
  [ ("width", "20px")
  , ("height", "20px")
  , ("margin", "40px")
  , ("position", "absolute")
  , ("left", (toString (char.targetX * 100)) ++ "px")
  , ("top", (toString (char.targetY * 100)) ++ "px")
  , ("backgroundColor", "lightcoral")
  , ("borderRadius", "3px")
  , ("boxShadow", "2px 2px 4px 1px gray")
  ]


  -- Utility functions

sign : Int -> Int
sign num =
  case num of
    0 -> 0
    n -> round (toFloat(n) / abs(toFloat(n)))
