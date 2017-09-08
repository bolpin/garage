module Drag exposing (..)

import Basics exposing (cos, pi, sin, sqrt)
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


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Model =
    { targetTerrain : Terrain
    , draggableTerrain : Terrain
    , draggablePosition : Position
    , drag : Maybe Drag
    , dbg : String
    }


startPosition : Position
startPosition =
    Position 100 100


initModel : ( Model, Cmd Msg )
initModel =
    ( { targetTerrain = Meadow
      , draggableTerrain = Sea
      , draggablePosition = startPosition
      , drag = Nothing
      , dbg = "Debug"
      }
    , Cmd.none
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
                            model.draggableTerrain

                        _ ->
                            model.targetTerrain

                newHexTerrain =
                    case inTarget xy of
                        True ->
                            nextTerrain model.draggableTerrain

                        _ ->
                            model.draggableTerrain
            in
            ( { model
                | draggablePosition = startPosition
                , draggableTerrain = newHexTerrain
                , targetTerrain = newTerrain
                , dbg = debugStr
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
    10.0


type Terrain
    = Forest
    | Hill
    | Meadow
    | Mountain
    | Sea


nextTerrain : Terrain -> Terrain
nextTerrain terrain =
    case terrain of
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
        [ svg [ viewBox "0 0 300 300", height "300px", width "300px" ]
            (List.concat [ viewTarget model, viewDraggable model ])
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
                , onMouseDown
                ]
                []
    in
    [ targetSvg ]


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
