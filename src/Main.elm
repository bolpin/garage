module Main exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (..)
import Svg exposing (Svg, svg, text, text_, rect)
import Svg.Attributes exposing (x, y, rotate, transform, width, height, viewBox, fill, rx, ry)
import Time exposing (Time, second)
import Html.Events exposing (onClick)


main =
    Html.program
        { init = initParkingSpots
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    List ParkingSpot


initParkingSpots : ( Model, Cmd Msg )
initParkingSpots =
    ( [ { id = 1
        , left = 10
        , top = 100
        , state = Booked
        }
      , { id = 2
        , left = 120
        , top = 100
        , state = Booked
        }
      , { id = 3
        , left = 230
        , top = 100
        , state = Booked
        }
      , { id = 24
        , left = 10
        , top = 300
        , state = Booked
        }
      , { id = 23
        , left = 120
        , top = 300
        , state = Booked
        }
      , { id = 22
        , left = 230
        , top = 300
        , state = Booked
        }
      ]
    , Cmd.none
    )


type alias ParkingSpot =
    { id : Int
    , left : Int
    , top : Int
    , state : State
    }


type State
    = Available
    | Booked



-- UPDATE


type Msg
    = Toggle Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle id ->
            ( model, Cmd.none )



-- VIEW


(=>) =
    (,)


view : Model -> Html Msg
view model =
    div []
        (List.map
            renderParkingSpot
            model
        )


renderParkingSpot : ParkingSpot -> Html Msg
renderParkingSpot spot =
    div
        [ style
            (styles spot.left spot.top spot.state)
        , onClick (Toggle spot.id)
        ]
        [ text (toString spot.id)
        ]


styles : Int -> Int -> State -> List ( String, String )
styles left top availability =
    [ "background-color" => availabilityColor availability
    , "cursor" => "move"
    , "width" => "100px"
    , "height" => "100px"
    , "border-radius" => "4px"
    , "position" => "absolute"
    , "left" => px left
    , "top" => px top
    , "color" => "white"
    , "display" => "flex"
    , "align-items" => "center"
    , "justify-content" => "center"
    , "font-size" => "26px"
    ]


availabilityColor : State -> String
availabilityColor state =
    case state of
        Available ->
            "#33cc33"

        Booked ->
            "#ff3300"


px : Int -> String
px num =
    toString num ++ "px"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
