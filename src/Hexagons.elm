module Hexagons exposing (..)

-- import Html.Events exposing (onClick)

import Array exposing (repeat, toList)
import Basics exposing (cos, pi, sin, sqrt)
import Dict exposing (..)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (href)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


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


nextState : State -> State
nextState state =
    case state of
        Forest ->
            Hill

        Hill ->
            Meadow

        Meadow ->
            Mountain

        Mountain ->
            Sea

        Sea ->
            Forest


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
    ( { cells = initCells }
    , Cmd.none
    )


initCells : List Cell
initCells =
    let
        xOrigin =
            20.0

        yOrigin =
            40.0

        xDelta =
            1.5 * radius

        yDelta =
            0.866 * radius

        initColumn : Float -> Float -> Int -> List Cell
        initColumn x y count =
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
            List.map2 createCell xs ys
    in
    List.concat
        [ initColumn xOrigin yOrigin 3
        , initColumn (xOrigin + xDelta) (yOrigin - yDelta) 4
        , initColumn (xOrigin + 2 * xDelta) (yOrigin - 2 * yDelta) 5
        , initColumn (xOrigin + 3 * xDelta) (yOrigin - yDelta) 4
        , initColumn (xOrigin + 4 * xDelta) yOrigin 3
        ]


createCell : Float -> Float -> Cell
createCell x y =
    { x = x
    , y = y
    , state = Forest
    }


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
    | ChangeState Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeState index ->
            ( { model | cells = incrementStateAt index model.cells }, Cmd.none )


incrementStateAt : Int -> List Cell -> List Cell
incrementStateAt idx cells =
    let
        changeState i cell =
            if i == idx then
                { cell | state = nextState cell.state }
            else
                cell
    in
    List.indexedMap changeState cells



-- Hexagon Size Property


radius : Float
radius =
    10.0



-- VIEW


commaSeparatedHexPoints : Cell -> String
commaSeparatedHexPoints cell =
    let
        hexPoint : Int -> String
        hexPoint i =
            let
                sides =
                    6.0

                calcX i =
                    cell.x + radius * cos (2.0 * pi * i / sides)

                calcY i =
                    cell.y + radius * sin (2.0 * pi * i / sides)
            in
            toString (calcX (toFloat i)) ++ "," ++ toString (calcY (toFloat i)) ++ " "
    in
    List.foldl (++) "" (List.map hexPoint (List.range 0 5))


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 300 300", width "900px" ] (hexGrid model)


hexGrid : Model -> List (Svg Msg)
hexGrid model =
    let
        drawHex idx cell =
            polygon
                [ points (commaSeparatedHexPoints cell)
                , Svg.Attributes.style ("fill:" ++ cellColor cell.state ++ ";stroke:black;stroke-width:1")
                , onClick (ChangeState idx)
                ]
                []
    in
    List.indexedMap drawHex model.cells


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
