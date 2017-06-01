module Main exposing (..)

import Html exposing (Html, div)
import Svg exposing (Svg, svg, text, text_, rect)
import Svg.Attributes exposing (x, y, rotate, transform, width, height, viewBox, fill, rx, ry)
import Time exposing (Time, second)
import Html.Events exposing (onClick)


main =
    Html.program
        { init = initGarage
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Garage


initGarage : ( Model, Cmd Msg )
initGarage =
    ( { height = 200
      , width = 300
      , parkingSpots = initParkingSpots
      }
    , Cmd.none
    )


initParkingSpots : List ParkingSpot
initParkingSpots =
    [ { number = 1
      , x = 10
      , y = 10
      , orientation = 15
      , state = Booked
      }
    ]


type alias Garage =
    { height : Int
    , width : Int
    , parkingSpots : List ParkingSpot
    }


type alias ParkingSpot =
    { number : Int
    , x : Int
    , y : Int
    , orientation : Int -- degrees
    , state : State
    }


type State
    = Available
    | Booked



-- UPDATE


type Msg
    = Left
    | Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Left ->
            ( { model | width = model.width + 10 }, Cmd.none )

        _ ->
            ( { model | width = model.width - 10 }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "600", height "600", viewBox "0 0 600 600" ]
            [ drawGarage
            , drawText
            , drawSpot
            ]
        ]


drawText : Svg Msg
drawText =
    text_ [ x "15", y "15", transform "rotate(30 20, 40)", fill "0x808080" ] [ text "Parking!" ]


drawGarage : Svg Msg
drawGarage =
    rect [ x "10", y "10", width (toString 500), height (toString 500), rx "5", ry "5", fill "#0B79CE", onClick Left ] []


drawSpot : Svg Msg
drawSpot =
    rect [ x "100", y "100", width "100", height "100", rx "5", ry "5", fill "#790B0E", onClick Right ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
