module Main exposing (..)

import Html exposing (Html, div, text, span, button)
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
    { measures : List Measure
    , timeSignature : TimeSignature
    , key : Key
    }



-- type alias Line =
--     { measures : List Measure
--     }
--


type alias Measure =
    { beats : List Beat
    }


type alias Beat =
    { chord : Chord
    , display : BeatDisplay
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


type BeatDisplay
    = Normal
    | Nashville
    | Slash


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
    { key = C, measures = initMeasures, timeSignature = FourFour }
        ! [ Cmd.none ]


initMeasures : List Measure
initMeasures =
    [ { beats = initBeats }
    , { beats = initBeats }
    , { beats = initBeats }
    , { beats = initBeats }
    ]


initBeats : List Beat
initBeats =
    [ { chord = initChord, display = Normal }
    , { chord = initChord, display = Slash }
    , { chord = initChord, display = Slash }
    , { chord = initChord, display = Slash }
    ]


initChord : Chord
initChord =
    { nashville = 1
    , chordType = Maj
    }



-- UPDATE


type Msg
    = NoOp
    | HalfStepUp Int
    | HalfStepDown Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HalfStepUp beatId ->
            ( model, Cmd.none )

        HalfStepDown beatId ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        px : Int -> String
        px num =
            toString num ++ "px"
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "left", "0px" )
                , ( "top", "0px" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                ]
            ]
            [ div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "row" )
                    , ( "flex-wrap", "wrap" )
                    , ( "justify-content", "left" )
                    , ( "position", "absolute" )
                    , ( "left", "0px" )
                    , ( "top", "0px" )
                    , ( "width", "100%" )
                    ]
                ]
                (List.map viewMeasure model.measures)
            ]


viewMeasure : Measure -> Html Msg
viewMeasure measure =
    div
        [ style
            [ ( "position", "relative" )
            , ( "text-align", "center" )
            , ( "cursor", "pointer" )
            , ( "border-style", "solid" )
            , ( "vertical-align", "middle" )
            , ( "width", "200px" )
            , ( "height", "40px" )
            , ( "margin", "10px" )
            , ( "padding", "10px" )
            , ( "border-color", "#CCC" )
            , ( "border-width", "0 2px 0 0" )
            , ( "vertical", "middle" )
            , ( "display", "flex" )
            , ( "flex-direction", "row" )
            , ( "flex-basis", "auto" )
            ]
        ]
        (List.map viewBeat measure.beats)


viewBeat : Beat -> Html Msg
viewBeat beat =
    span
        [ style
            [ ( "position", "relative" )
            , ( "text-align", "center" )
            , ( "line-height", "40px" )
            , ( "flex-basis", "0" )
            , ( "flex-grow", "1" )
            ]
        ]
        [ text <| chordString beat
        ]


nashvilleToName : Int -> Key -> String
nashvilleToName nashville key =
    "TODO"



-- viewHalfStepUpControl : Beat -> Html Msg
-- viewHalfStepUpControl beat =
--     button [ onClick HalfStepUp beat.id ]
--         [ text "⬆" ]
--
--
-- viewHalfStepDownControl : Beat -> Html Msg
-- viewHalfStepDownControl beat =
--     button [ onClick HalfStepDown beat.id ]
--         [ text "⬇" ]


chordString : Beat -> String
chordString beat =
    let
        primary =
            "C"

        secondary =
            "Maj"

        chordName =
            primary ++ secondary

        slash =
            "/"

        rest =
            "."
    in
        case ( beat.chord, beat.display ) of
            ( chord, Normal ) ->
                "AbMin7"

            ( chord, Nashville ) ->
                toString chord.nashville

            ( _, Slash ) ->
                "/"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
