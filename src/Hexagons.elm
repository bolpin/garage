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
-- hexGrid : List (Html Msg)
-- hexGrid =
--     let
--         longDiameter =
--             sqrt 3 * radius
--     in
--     [ drawHex 40.0 50.0 ]


radius : Float
radius =
    10.0


hexPoints : Float -> Float -> String
hexPoints xOffset yOffset =
    let
        hexPoint : Int -> String
        hexPoint i =
            let
                sides =
                    6

                calcX i =
                    xOffset + radius * cos (2.0 * pi * i / toFloat sides)

                calcY i =
                    yOffset + radius * sin (2.0 * pi * i / toFloat sides)
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
        x =
            20.0

        y =
            40.0

        xDelta =
            15.0

        yDelta =
            8.66
    in
    List.concat
        [ drawColumn x y 4
        , drawColumn (x + xDelta) (y - yDelta) 5
        , drawColumn (x + 2 * xDelta) (y - 2 * yDelta) 6
        , drawColumn (x + 3 * xDelta) (y - yDelta) 5
        , drawColumn (x + 4 * xDelta) y 4
        ]



-- svgDefs : List (Svg Msg)
-- svgDefs =
--     [ defs
--         [ id "tree-image" ]
--         [ Svg.pattern []
--             [ Html.img [ width "300", height "300", href "http://placekitten.com/306/306" ] []
--             ]
--         ]
--     ]


drawColumn : Float -> Float -> Int -> List (Svg Msg)
drawColumn x y count =
    let
        yDelta i =
            2 * 8.66 * i

        addX a =
            a + x

        addY a =
            a + y

        xs =
            Array.repeat count x
                |> Array.toList

        ys =
            List.range 0 count
                |> List.map toFloat
                |> List.map yDelta
                |> List.map addY
    in
    List.map2 drawHex xs ys


drawHex : Float -> Float -> Html Msg
drawHex offsetX offsetY =
    polygon [ points (hexPoints offsetX offsetY), Svg.Attributes.style ("fill:" ++ hexColor offsetX offsetY ++ ";stroke:black;stroke-width:1") ] []



-- polygon [ points (hexPoints offsetX offsetY), Svg.Attributes.style "fill:url('#tree-image');stroke:black;stroke-width:1" ] []
-- polygon [ points (hexPoints offsetX offsetY), fill ( hexColor offsetX offsetY) ] []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
