module Main exposing (..)

import Html exposing (Html, div, text, span, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dict exposing (..)


main =
    Html.program
        { init = initTune
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { beats : Dict ( Int, Int ) Beat
    }


type Tone
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


type ChordFlavor
    = Maj
    | Min
    | Seven
    | Diminished
    | Aug5
    | Maj7
    | Min7


type alias Beat =
    { chord : Maybe Chord }


type alias Chord =
    { tone : Int
    , chordFlavor : Maybe ChordFlavor
    }


initTune : ( Model, Cmd Msg )
initTune =
    { beats = initBeats }
        ! [ Cmd.none ]


initBeats : Dict ( Int, Int ) Beat
initBeats =
    Dict.fromList
        [ ( ( 0, 0 ), { chord = Just { tone = 0, chordFlavor = Nothing } } )
        , ( ( 0, 1 ), { chord = Just { tone = 0, chordFlavor = Nothing } } )
        , ( ( 0, 2 ), { chord = Just { tone = 0, chordFlavor = Nothing } } )
        , ( ( 0, 3 ), { chord = Just { tone = 0, chordFlavor = Nothing } } )
        ]



-- initBeat : Tone -> Beat
-- initBeat tone =


initChord : Int -> Chord
initChord t =
    { tone = t
    , chordFlavor = Just Maj
    }


intToTone : Int -> Maybe Tone
intToTone n =
    case n of
        0 ->
            Just C

        1 ->
            Just CSharp

        2 ->
            Just D

        3 ->
            Just DSharp

        4 ->
            Just E

        5 ->
            Just F

        6 ->
            Just FSharp

        7 ->
            Just G

        8 ->
            Just GSharp

        9 ->
            Just A

        10 ->
            Just ASharp

        11 ->
            Just B

        _ ->
            Nothing



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
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
                (List.map viewBeat (Dict.values model.beats))
            ]



-- viewMeasure : List Beat -> Html Msg
-- viewMeasure measure =
--     div
--         [ style
--             [ ( "position", "relative" )
--             , ( "text-align", "center" )
--             , ( "cursor", "pointer" )
--             , ( "border-style", "solid" )
--             , ( "vertical-align", "middle" )
--             , ( "width", "200px" )
--             , ( "height", "40px" )
--             , ( "margin", "10px" )
--             , ( "padding", "10px" )
--             , ( "border-color", "#CCC" )
--             , ( "border-width", "0 2px 0 0" )
--             , ( "vertical", "middle" )
--             , ( "display", "flex" )
--             , ( "flex-direction", "row" )
--             , ( "flex-basis", "auto" )
--             ]
--         ]
--         []
--
--
--
-- -- (List.map viewBeat measure.beats)
--


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


chordString : Beat -> String
chordString beat =
    case beat.chord of
        Just chord ->
            chordToString chord

        Nothing ->
            "/"


chordToString : Chord -> String
chordToString chord =
    let
        primary tone =
            case (intToTone chord.tone) of
                Just C ->
                    "C"

                Just CSharp ->
                    "C#"

                Just D ->
                    "D"

                Just DSharp ->
                    "D#"

                Just E ->
                    "E"

                Just F ->
                    "E#"

                Just FSharp ->
                    "F"

                Just G ->
                    "G"

                Just GSharp ->
                    "G#"

                Just A ->
                    "A"

                Just ASharp ->
                    "A#"

                Just B ->
                    "B"

                Nothing ->
                    "/"

        secondary chordFlavor =
            case chordFlavor of
                Nothing ->
                    ""

                Just Maj ->
                    ""

                Just Min ->
                    "min"

                Just Seven ->
                    "7"

                Just Diminished ->
                    "dim"

                Just Aug5 ->
                    "+5"

                Just Maj7 ->
                    "maj7"

                Just Min7 ->
                    "min7"
    in
        (primary chord.tone) ++ (secondary chord.chordFlavor)


viewHalfStepUpControl : Int -> Int -> Html Msg
viewHalfStepUpControl measureId beatId =
    button [ onClick (NoOp) ]
        [ text "⬆" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
