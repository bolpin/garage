module Hexagons exposing (..)

import Array exposing (repeat, toList)
import Basics exposing (cos, pi, sin, sqrt)
import Dict exposing (..)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (href)
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


type State
    = Forest
    | Hill
    | Meadow
    | Mountain
    | Sea


type alias Cell =
    { x : Float
    , y : Float
    , state : State
    }


type alias Model =
    { cells : List Cell
    }


initModel : ( Model, Cmd Msg )
initModel =
    ( { cells = [] }
    , Cmd.none
    )


cellColor : State -> String
cellColor state =
    case state of
        Forest ->
            "green"

        Hill ->
            "red"

        Meadow ->
            "yellow"

        Mountain ->
            "gray"

        _ ->
            "blue"



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



-- Hexagon Properties


hexRadius : Float
hexRadius =
    10.0


xDelta : Float
xDelta =
    1.5 * hexRadius


yDelta : Float
yDelta =
    0.866 * hexRadius



-- VIEW


commaSeparatedhexPoints : Float -> Float -> String
commaSeparatedhexPoints xOffset yOffset =
    let
        hexPoint : Int -> String
        hexPoint i =
            let
                sides =
                    6.0

                calcX i =
                    xOffset + hexRadius * cos (2.0 * pi * i / sides)

                calcY i =
                    yOffset + hexRadius * sin (2.0 * pi * i / sides)
            in
            toString (calcX (toFloat i)) ++ "," ++ toString (calcY (toFloat i)) ++ " "
    in
    List.foldl (++) "" (List.map hexPoint (List.range 0 5))


hexColor : Float -> Float -> String
hexColor a b =
    "#AACCEE"


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 300 300", width "900px" ]
        hexGrid


hexGrid : List (Svg Msg)
hexGrid =
    let
        xOrigin =
            20.0

        yOrigin =
            40.0
    in
    List.concat
        [ drawColumn xOrigin yOrigin 4
        , drawColumn (xOrigin + xDelta) (yOrigin - yDelta) 5
        , drawColumn (xOrigin + 2 * xDelta) (yOrigin - 2 * yDelta) 6
        , drawColumn (xOrigin + 3 * xDelta) (yOrigin - 3 * yDelta) 7
        , drawColumn (xOrigin + 4 * xDelta) (yOrigin - 2 * yDelta) 6
        , drawColumn (xOrigin + 5 * xDelta) (yOrigin - yDelta) 5
        , drawColumn (xOrigin + 6 * xDelta) yOrigin 4
        ]


drawColumn : Float -> Float -> Int -> List (Svg Msg)
drawColumn x y count =
    let
        xs =
            Array.repeat count x
                |> Array.toList

        ys =
            List.range 0 count
                |> List.map toFloat
                |> List.map (\y -> 2 * yDelta * y)
                |> List.map (\yOffset -> y + yOffset)
    in
    List.map2 drawHex xs ys



-- draw a Hexagon centered at the point (x, y)


drawHex : Float -> Float -> Html Msg
drawHex x y =
    polygon
        [ points (commaSeparatedhexPoints x y)
        , Svg.Attributes.style ("fill:" ++ hexColor x y ++ ";stroke:black;stroke-width:1")
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
