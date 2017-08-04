module Main exposing (..)

import Dict exposing (..)
import Html exposing (Html, button, div, span, text)
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
    { beats : Dict Int Beat
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


type Flavor
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
    , flavor : Flavor
    }


initTune : ( Model, Cmd Msg )
initTune =
    { beats = initBeats }
        ! [ Cmd.none ]


initBeats : Dict Int Beat
initBeats =
    Dict.fromList
        [ ( 0, { chord = Just { tone = 1, flavor = Maj } } )
        , ( 1, { chord = Nothing } )
        , ( 2, { chord = Just { tone = 2, flavor = Min } } )
        , ( 3, { chord = Just { tone = 1, flavor = Aug5 } } )
        ]


initChord : Int -> Chord
initChord t =
    { tone = t
    , flavor = Maj
    }


intToTone : Int -> Tone
intToTone n =
    case n of
        0 ->
            C

        1 ->
            CSharp

        2 ->
            D

        3 ->
            DSharp

        4 ->
            E

        5 ->
            F

        6 ->
            FSharp

        7 ->
            G

        8 ->
            GSharp

        9 ->
            A

        10 ->
            ASharp

        11 ->
            B

        _ ->
            C



-- UPDATE


type Msg
    = Increment Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment index ->
            let
                incrementBeat : Maybe Beat -> Maybe Beat
                incrementBeat beat =
                    case beat of
                        Nothing ->
                            Just { chord = Nothing }

                        Just b ->
                            Just { chord = increment b.chord }
            in
            ( { model | beats = Dict.update index incrementBeat model.beats }, Cmd.none )


increment : Maybe Chord -> Maybe Chord
increment chord =
    case chord of
        Nothing ->
            Nothing

        Just c ->
            case c.tone of
                11 ->
                    Just { tone = 0, flavor = c.flavor }

                _ ->
                    Just { tone = c.tone + 1, flavor = c.flavor }



-- VIEW


view : Model -> Html Msg
view model =
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
            [ viewMeasures model
            , viewControls model
            ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    div []
        [ button [ onClick (Increment 3) ] [ text "+" ]
        , viewHalfStepUpControl 0 0
        ]


viewMeasures : Model -> Html Msg
viewMeasures model =
    div []
        (List.map viewMeasure (partitionIntoMeasuresList model.beats))


partitionIntoMeasuresList : Dict Int Beat -> List (List Beat)
partitionIntoMeasuresList allBeats =
    let
        keys : List Int
        keys =
            Dict.keys allBeats
    in
    [ Dict.values allBeats ]


viewMeasure : List Beat -> Html Msg
viewMeasure beats =
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
        (List.map viewBeat beats)


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
            case intToTone chord.tone of
                C ->
                    "C"

                CSharp ->
                    "C#"

                D ->
                    "D"

                DSharp ->
                    "D#"

                E ->
                    "E"

                F ->
                    "F"

                FSharp ->
                    "F#"

                G ->
                    "G"

                GSharp ->
                    "G#"

                A ->
                    "A"

                ASharp ->
                    "A#"

                B ->
                    "B"

        secondary flavor =
            case flavor of
                Maj ->
                    "maj"

                Min ->
                    "min"

                Seven ->
                    "7"

                Diminished ->
                    "dim"

                Aug5 ->
                    "+5"

                Maj7 ->
                    "maj7"

                Min7 ->
                    "min7"
    in
    primary chord.tone ++ secondary chord.flavor


viewHalfStepUpControl : Int -> Int -> Html Msg
viewHalfStepUpControl measureId beatId =
    button [ onClick (Increment ((measureId * 4) + beatId)) ]
        [ text "⬆" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
