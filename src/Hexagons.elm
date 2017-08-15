module Hexagons exposing (..)

-- import Html.Attributes exposing (..)

import Basics exposing (cos, pi, sin)
import Dict exposing (..)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Html.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Hex x, y, size
-- HexGrid x, y, width, height, size


type alias Model =
    { diagonalLength : Int
    }


initModel : ( Model, Cmd Msg )
initModel =
    ( { diagonalLength = 10 }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | HoverHex Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HoverHex x y ->
            ( model, Cmd.none )



-- VIEW


hexPoints : Int -> Int -> String
hexPoints xOffset yOffset =
    let
        hexPoint : Int -> String
        hexPoint i =
            let
                radius =
                    10.1

                sides =
                    6

                calcX i =
                    toFloat xOffset + radius * cos (2 * pi * i / sides)

                calcY i =
                    toFloat yOffset + radius * sin (2 * pi * i / sides)
            in
            toString (calcX (toFloat i)) ++ "," ++ toString (calcY (toFloat i)) ++ " "
    in
    List.foldl (++) "" (List.map hexPoint (List.range 0 5))


hexColor : Int -> Int -> String
hexColor a b =
    "#EE" ++ toString a ++ toString b


view : Model -> Html Msg
view model =
    let
        px : Int -> String
        px num =
            toString num ++ "px"
    in
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ drawHex 20 40, drawHex 20 57, drawHex 35 49 ]


drawHex : Int -> Int -> Html Msg
drawHex offsetX offsetY =
    polygon [ points (hexPoints offsetX offsetY), Svg.Attributes.style ("fill:" ++ hexColor offsetX offsetY ++ ";stroke:black;stroke-width:1") ] []



-- polygon [ points (hexPoints offsetX offsetY), fill ( hexColor offsetX offsetY) ] []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
