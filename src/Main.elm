module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Events
import Element exposing (Element, alignRight, column, el, fill, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode


type Operator
    = Plus
    | Minus
    | Multiply
    | Divide
    | Equal


type alias Digit =
    Int


type alias Model =
    { total : Float, operator : Maybe Operator, entry : String }


type Key
    = Character Char
    | Control String


type KeyValue
    = DigitKey Digit
    | OperatorKey Operator
    | FloatingKey
    | ClearKey


type Msg
    = PressedDigit Digit
    | PressedOperator Operator
    | PressedFloating
    | PressedKey Key
    | Clear


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { total = 0
      , operator = Nothing
      , entry = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedDigit digit ->
            applyDigit digit model

        PressedOperator operator ->
            applyOperator operator model

        PressedFloating ->
            applyFloating model

        PressedKey key ->
            case toKeyValue key of
                Just (DigitKey digit) ->
                    applyDigit digit model

                Just (OperatorKey operator) ->
                    applyOperator operator model

                Just FloatingKey ->
                    applyFloating model

                Just ClearKey ->
                    applyClear

                Nothing ->
                    ( model, Cmd.none )

        Clear ->
            applyClear


applyDigit : Digit -> Model -> ( Model, Cmd Msg )
applyDigit digit model =
    ( { model | entry = model.entry ++ String.fromInt digit }, Cmd.none )


applyOperator : Operator -> Model -> ( Model, Cmd Msg )
applyOperator operator model =
    ( { model | total = calculate model, operator = Just operator, entry = "" }, Cmd.none )


applyFloating : Model -> ( Model, Cmd Msg )
applyFloating model =
    if String.contains "." model.entry then
        ( model, Cmd.none )

    else if model.entry == "" then
        ( { model | entry = "0." }, Cmd.none )

    else
        ( { model | entry = model.entry ++ "." }, Cmd.none )


applyClear : ( Model, Cmd Msg )
applyClear =
    init ()


toKeyValue : Key -> Maybe KeyValue
toKeyValue key =
    case key of
        Character char ->
            case char of
                '+' ->
                    Just (OperatorKey Plus)

                '-' ->
                    Just (OperatorKey Minus)

                '*' ->
                    Just (OperatorKey Multiply)

                '/' ->
                    Just (OperatorKey Divide)

                '=' ->
                    Just (OperatorKey Equal)

                '.' ->
                    Just FloatingKey

                'c' ->
                    Just ClearKey

                _ ->
                    let
                        str =
                            String.fromChar char
                    in
                    if String.contains str "0123456789" then
                        Just
                            (DigitKey
                                (String.toInt str |> Maybe.withDefault 0)
                            )

                    else
                        Nothing

        Control name ->
            case name of
                "Enter" ->
                    Just (OperatorKey Equal)

                "Backspace" ->
                    Just ClearKey

                "Delete" ->
                    Just ClearKey

                _ ->
                    Nothing


calculate : Model -> Float
calculate model =
    let
        entryValue =
            String.toFloat model.entry |> Maybe.withDefault 0
    in
    case model.operator of
        Just Plus ->
            model.total + entryValue

        Just Minus ->
            model.total - entryValue

        Just Multiply ->
            model.total * entryValue

        Just Divide ->
            model.total / entryValue

        Just Equal ->
            model.total

        Nothing ->
            entryValue


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            PressedKey (Character char)

        _ ->
            PressedKey (Control string)


view : Model -> Html Msg
view model =
    Element.layout [ padding 10, Background.color theme.mainBg ]
        (column [ spacing 10 ]
            [ row [ width fill, padding 10, Background.color theme.displayBg ] [ el [ alignRight ] (text (toDisplayValue model)) ]
            , row
                [ spacing 10 ]
                [ button "7" theme.digitBg (PressedDigit 7)
                , button "8" theme.digitBg (PressedDigit 8)
                , button "9" theme.digitBg (PressedDigit 9)
                , button "÷" theme.operatorBg (PressedOperator Divide)
                ]
            , row
                [ spacing 10 ]
                [ button "4" theme.digitBg (PressedDigit 4)
                , button "5" theme.digitBg (PressedDigit 5)
                , button "6" theme.digitBg (PressedDigit 6)
                , button "×" theme.operatorBg (PressedOperator Multiply)
                ]
            , row
                [ spacing 10 ]
                [ button "1" theme.digitBg (PressedDigit 1)
                , button "2" theme.digitBg (PressedDigit 2)
                , button "3" theme.digitBg (PressedDigit 3)
                , button "−" theme.operatorBg (PressedOperator Minus)
                ]
            , row
                [ spacing 10 ]
                [ button "0" theme.digitBg (PressedDigit 0)
                , button "." theme.operatorBg PressedFloating
                , button "=" theme.equalBg (PressedOperator Equal)
                , button "+" theme.operatorBg (PressedOperator Plus)
                ]
            , row
                [ spacing 10 ]
                [ button "C" theme.operatorBg Clear ]
            ]
        )


theme =
    { mainBg =
        rgb255 57 55 56
    , digitBg =
        rgb255 0 0 0
    , equalBg =
        rgb255 250 167 80
    , operatorBg =
        rgb255 81 56 44
    , displayBg =
        rgb255 207 208 201
    , fontColor =
        rgb255 255 255 255
    }


toDisplayValue : Model -> String
toDisplayValue model =
    if model.entry == "" then
        String.fromFloat model.total

    else
        model.entry


button : String -> Element.Color -> Msg -> Element Msg
button label bg msg =
    Input.button [ width <| Element.px 50, height <| Element.px 50, Border.rounded 25, Background.color bg, Font.color theme.fontColor, Font.center ] { label = text label, onPress = Just msg }
