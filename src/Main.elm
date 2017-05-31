module Main exposing (..)

import Html exposing (Html, div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


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
      , orientation = 0
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
    }



-- UPDATE


type Msg
    = Increment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "300", height "300", viewBox "0 0 320 320" ]
            [ garageRect, spotRect ]
        ]


garageRect : Svg msg
garageRect =
    rect [ x "10", y "10", width (toString 100), height (toString 100), rx "5", ry "5", fill "#0B79CE" ] []


spotRect : Svg msg
spotRect =
    rect [ x "100", y "100", width "100", height "100", rx "5", ry "5", fill "#790B0E" ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
