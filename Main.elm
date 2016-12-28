module Main exposing (..)

import Html exposing (Html, div)
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

initArena : Arena
initArena =
  [ [ Wall, Wall, Wall, Wall, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Wall ]
  , [ Wall, Wall, Wall, Gr 1, Wall ]
  , [ Wall, Gr 1, Wall, Gr 1, Wall ]
  , [ Wall, Gr 1, Gr 1, Gr 1, Wall ]
  , [ Wall, Wall, Wall, Wall, Wall ]
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

view : Model -> Html.Html Msg
view model =
  div [] []
