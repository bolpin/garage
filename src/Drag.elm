module Drag exposing (..)

import Basics exposing (cos, pi, sin, sqrt)
import Dict exposing (Dict, get)
import Html exposing (Html, div)
import Html.Attributes exposing (href)
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg exposing (Attribute, Svg, circle, polygon, svg)
import Svg.Attributes exposing (cx, cy, height, points, r, viewBox, width)
import VirtualDom exposing (on)


main =
    Html.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Cell =
    { x : Float
    , y : Float
    , terrain : Terrain
    }


type alias Target =
    { x : Int
    , y : Int
    , r : Int
    , terrain : Terrain
    }


type alias Drag =
    { start : Position
    , current : Position
    , cellIndex : Int
    }


type alias Model =
    { cells : Dict Int Cell
    , targetTerrain : Terrain
    , drag : Maybe Drag
    }


initModel : ( Model, Cmd Msg )
initModel =
    ( { cells = initCells
      , targetTerrain = Placeholder
      , drag = Nothing
      }
    , Cmd.none
    )


initCells : Dict Int Cell
initCells =
    Dict.fromList
        (List.indexedMap
            (,)
            [ { x = 30, y = 20, terrain = Forest }
            , { x = 60, y = 20, terrain = Mountain }
            , { x = 90, y = 20, terrain = Sea }
            , { x = 120, y = 20, terrain = Hill }
            , { x = 150, y = 20, terrain = Meadow }
            ]
        )


cellColor : Terrain -> String
cellColor terrain =
    case terrain of
        Forest ->
            "green"

        Hill ->
            "red"

        Meadow ->
            "yellow"

        Mountain ->
            "gray"

        Sea ->
            "blue"

        _ ->
            "black"


closestCellKey : Position -> Dict Int Cell -> Int
closestCellKey dragPosition cells =
    let
        fakeCell =
            { x = toFloat dragPosition.x, y = toFloat dragPosition.y, terrain = Placeholder }

        distance cell1 cell2 =
            sqrt ((cell2.x - cell1.x) ^ 2.0 + (cell2.y - cell1.y) ^ 2.0)

        distances =
            List.map (distance fakeCell) (Dict.values cells)
    in
    minIndex distances


minIndex : List Float -> Int
minIndex xs =
    let
        aux ( i, x ) ( i2, x2 ) =
            case x2 of
                Nothing ->
                    ( i, Just x )

                Just x2_ ->
                    if x > x2_ then
                        ( i2, x2 )
                    else
                        ( i, Just x )
    in
    xs
        |> List.indexedMap (,)
        |> List.foldl aux ( -1, Nothing )
        |> Tuple.first


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart xy ->
            ( { model
                | drag = Just (Drag xy xy (closestCellKey xy model.cells))
              }
            , Cmd.none
            )

        DragAt xy ->
            ( { model
                | drag = Maybe.map (\{ start, current, cellIndex } -> Drag start xy cellIndex) model.drag
              }
            , Cmd.none
            )

        DragEnd xy ->
            let
                debugStr =
                    toString targetX
                        ++ " "
                        ++ toString
                            targetY
                        ++ " "
                        ++ toString
                            xy.x
                        ++ " "
                        ++ toString
                            xy.y

                dist =
                    (targetX - toFloat xy.x) ^ 2.0 + (targetY - toFloat xy.y) ^ 2.0

                inTarget xy =
                    targetRadius ^ 2.0 > dist

                newTerrain =
                    case inTarget xy of
                        True ->
                            case model.drag of
                                Nothing ->
                                    model.targetTerrain

                                Just drag ->
                                    case Dict.get drag.cellIndex model.cells of
                                        Nothing ->
                                            model.targetTerrain

                                        Just cell ->
                                            cell.terrain

                        _ ->
                            model.targetTerrain
            in
            ( { model
                | targetTerrain = newTerrain
                , drag = Nothing
              }
            , Cmd.none
            )



-- VIEW


targetRadius : Float
targetRadius =
    85.0


targetX : Float
targetX =
    200.0


targetY : Float
targetY =
    200.0


radius : Float
radius =
    15.0


type Terrain
    = Forest
    | Hill
    | Meadow
    | Mountain
    | Sea
    | Placeholder


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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg [ viewBox "0 0 300 300", height "300px", width "300px" ]
            (List.concat [ viewTarget model, viewCells model ])
        , Html.text "Drag the hexagon into the circle to change the circle's color."
        ]


viewTarget : Model -> List (Svg Msg)
viewTarget model =
    let
        targetSvg =
            circle
                [ cx (toString targetX)
                , cy (toString targetY)
                , r (toString targetRadius)
                , Svg.Attributes.style ("fill:" ++ cellColor model.targetTerrain ++ ";stroke:black;stroke-width:1")
                ]
                []
    in
    [ targetSvg ]


viewCells : Model -> List (Svg Msg)
viewCells model =
    case model.drag of
        Nothing ->
            List.map viewCell (Dict.values model.cells)

        Just d ->
            case Dict.get d.cellIndex model.cells of
                Nothing ->
                    []

                Just c ->
                    [ viewDraggedCell c model.drag ]


viewCell : Cell -> Svg Msg
viewCell cell =
    polygon
        [ points (commaSeparatedHexPoints cell)
        , Svg.Attributes.style ("fill:" ++ cellColor cell.terrain ++ ";stroke:black;stroke-width:1")
        , onMouseDown
        ]
        []


viewDraggedCell : Cell -> Maybe Drag -> Svg Msg
viewDraggedCell cell drag =
    let
        realPosition =
            getPosition cell.x cell.y drag

        draggedCell =
            { x = toFloat realPosition.x
            , y = toFloat realPosition.y
            , terrain = cell.terrain
            }

        draggedSvg =
            polygon
                [ points (commaSeparatedHexPoints draggedCell)
                , Svg.Attributes.style ("fill:" ++ cellColor draggedCell.terrain ++ ";stroke:black;stroke-width:1")
                , onMouseDown
                ]
                []
    in
    draggedSvg


getPosition : Float -> Float -> Maybe Drag -> Position
getPosition x y drag =
    let
        xy =
            { x = round x, y = round y }
    in
    case drag of
        Nothing ->
            xy

        Just { start, current } ->
            Position
                (xy.x + current.x - start.x)
                (xy.y + current.y - start.y)



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
