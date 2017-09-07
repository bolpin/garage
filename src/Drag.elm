module Hexagons exposing (..)

-- import Html.Events exposing (on)

import Array exposing (repeat, toList)
import Basics exposing (cos, pi, sin, sqrt)
import Dict exposing (..)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (href)
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import VirtualDom exposing (on)


main =
    Html.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Terrain
    = Forest
    | Meadow
    | Sea


type alias Cell =
    { x : Float
    , y : Float
    , terrain : Terrain
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Model =
    { draggableTerrain : Terrain
    , draggablePosition : Position
    , drag : Maybe Drag
    }


initModel : ( Model, Cmd Msg )
initModel =
    ( { draggableTerrain = Sea
      , draggablePosition = Position 100 100
      , drag = Nothing
      }
    , Cmd.none
    )


cellColor : Terrain -> String
cellColor terrain =
    case terrain of
        Forest ->
            "green"

        Meadow ->
            "yellow"

        _ ->
            "blue"



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart xy ->
            ( { model
                | drag = Just (Drag xy xy)
              }
            , Cmd.none
            )

        DragAt xy ->
            ( { model
                | drag = Maybe.map (\{ start } -> Drag start xy) model.drag
              }
            , Cmd.none
            )

        DragEnd _ ->
            ( { model
                | draggablePosition = getPosition model.draggablePosition model.drag
                , drag = Nothing
              }
            , Cmd.none
            )


radius : Float
radius =
    30.0



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
    div []
        [ svg [ viewBox "0 0 900 900", height "900px", width "900px" ]
            (List.concat [ viewDraggable model ])
        ]


viewDraggable : Model -> List (Svg Msg)
viewDraggable model =
    let
        realPosition =
            getPosition model.draggablePosition model.drag

        draggableCell =
            { x = toFloat realPosition.x
            , y = toFloat realPosition.y
            , terrain = model.draggableTerrain
            }

        draggableSvg =
            polygon
                [ points (commaSeparatedHexPoints draggableCell)
                , Svg.Attributes.style ("fill:" ++ cellColor draggableCell.terrain ++ ";stroke:black;stroke-width:1")
                , onMouseDown
                ]
                []
    in
    [ draggableSvg ]


getPosition : Position -> Maybe Drag -> Position
getPosition position drag =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)
