module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
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


type alias ParkingSpot =
    { id : Int
    , left : Int
    , top : Int
    , state : State
    }


type State
    = Available
    | Booked


initParkingSpots : ( Model, Cmd Msg )
initParkingSpots =
    ( [ { id = 101, left = 10, top = 100, state = Booked }
      , { id = 102, left = 120, top = 100, state = Booked }
      , { id = 103, left = 230, top = 100, state = Booked }
      ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = Toggle Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle id ->
            ( toggle id model, Cmd.none )


toggle : Int -> List ParkingSpot -> Model
toggle spotId list =
    let
        toggleSpot : ParkingSpot -> ParkingSpot
        toggleSpot spot =
            case spot.state of
                Booked ->
                    { spot | state = Available }

                Available ->
                    { spot | state = Booked }
    in
    case list of
        [] ->
            list

        x :: xs ->
            if x.id == spotId then
                toggleSpot x :: xs
            else
                x :: toggle spotId xs



-- VIEW


view : Model -> Html Msg
view model =
    let
        parkingSpotStyles : Int -> Int -> State -> List ( String, String )
        parkingSpotStyles left top availability =
            [ ( "background-color", availabilityColor availability )
            , ( "cursor", "pointer" )
            , ( "width", "100px" )
            , ( "height", "100px" )
            , ( "border-radius", "4px" )
            , ( "position", "absolute" )
            , ( "left", px left )
            , ( "top", px top )
            , ( "color", "white" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            , ( "font-size", "26px" )
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

        renderParkingSpot : ParkingSpot -> Html Msg
        renderParkingSpot spot =
            div
                [ style (parkingSpotStyles spot.left spot.top spot.state)
                , onClick (Toggle spot.id)
                ]
                [ text (toString spot.id)
                ]
    in
    div []
        (List.map
            renderParkingSpot
            model
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
