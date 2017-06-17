module Main exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Html.program
        { init = initTune
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { lines : List Line
    , timeSignature : TimeSignature
    , key : Key
    }


type alias Line =
    { yOffset : Int
    , measures : List Measure
    }


type alias Measure =
    { beats : List Beat
    }


type alias Beat =
    { chord : Chord
    , display : BeatDisplayType
    }


type alias Chord =
    { nashville : Int
    , chordType : ChordType
    }


type ChordType
    = Maj
    | Min
    | Seven
    | Diminished
    | Aug5
    | Maj7
    | Min7


type TimeSignature
    = FourFour


type BeatDisplayType
    = ChordName
    | ChordNashville
    | Slash
    | Rest


type State
    = Available
    | Booked


type Key
    = C
    | CSharp
    | D
    | DSharp
    | E
    | F
    | FSharp
    | G
    | GSharp
    | A
    | ASharp
    | B


initTune : ( Model, Cmd Msg )
initTune =
    ( [ { measures = [], timeSignature = FourFour }
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


toggle : Int -> List Measure -> Model
toggle measureId list =
    let
        toggleMeasure measure =
            case measure.state of
                Booked ->
                    { measure | state = Available }

                Available ->
                    { measure | state = Booked }
    in
        case list of
            [] ->
                list

            x :: xs ->
                if x.id == measureId then
                    toggleMeasure x :: xs
                else
                    x :: toggle measureId xs



-- VIEW


view : Model -> Html Msg
view model =
    let
        px : Int -> String
        px num =
            toString num ++ "px"

        renderMeasure : Measure -> Html Msg
        renderMeasure measure =
            div
                [ style (measureStyles measure.left measure.top measure.state)
                , onClick (Toggle measure.id)
                ]
                [ text (toString measure.id)
                ]
    in
        div []
            (List.map
                renderMeasure
                model
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
