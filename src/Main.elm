module Main exposing (Msg(..), main, toDisplayValue, update, view)

import Browser
import Browser.Events
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (acceptCharset)
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
            updateDigit digit model

        PressedOperator operator ->
            updateOperator operator model

        PressedFloating ->
            updateFloating model

        PressedKey key ->
            case key of
                Character char ->
                    let
                        value =
                            keyValueFromChar char
                    in
                    case value of
                        Just (DigitKey digit) ->
                            updateDigit digit model

                        Just (OperatorKey operator) ->
                            updateOperator operator model

                        Just FloatingKey ->
                            updateFloating model

                        Just ClearKey ->
                            updateClear

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Clear ->
            updateClear


updateDigit : Digit -> Model -> ( Model, Cmd Msg )
updateDigit digit model =
    ( { model | entry = model.entry ++ String.fromInt digit }, Cmd.none )


updateOperator : Operator -> Model -> ( Model, Cmd Msg )
updateOperator operator model =
    ( { model | total = calculate model, operator = Just operator, entry = "" }, Cmd.none )


updateFloating : Model -> ( Model, Cmd Msg )
updateFloating model =
    if String.contains "." model.entry then
        ( model, Cmd.none )

    else if model.entry == "" then
        ( { model | entry = "0." }, Cmd.none )

    else
        ( { model | entry = model.entry ++ "." }, Cmd.none )


updateClear : ( Model, Cmd Msg )
updateClear =
    init ()


type KeyValue
    = DigitKey Digit
    | OperatorKey Operator
    | FloatingKey
    | ClearKey


keyValueFromChar : Char -> Maybe KeyValue
keyValueFromChar char =
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
            if String.contains (String.fromChar char) "0123456789" then
                Just
                    (DigitKey
                        (toIntOrZero (String.fromChar char))
                    )

            else
                Nothing


toFloatOrZero : String -> Float
toFloatOrZero s =
    let
        f =
            String.toFloat s
    in
    case f of
        Just x ->
            x

        Nothing ->
            0


toIntOrZero : String -> Int
toIntOrZero s =
    let
        i =
            String.toInt s
    in
    case i of
        Just x ->
            x

        Nothing ->
            0


calculate : Model -> Float
calculate model =
    case model.operator of
        Just Plus ->
            model.total + toFloatOrZero model.entry

        Just Minus ->
            model.total - toFloatOrZero model.entry

        Just Multiply ->
            model.total * toFloatOrZero model.entry

        Just Divide ->
            model.total / toFloatOrZero model.entry

        Just Equal ->
            model.total

        Nothing ->
            toFloatOrZero model.entry


toDisplayValue : Model -> String
toDisplayValue model =
    if model.entry == "" then
        String.fromFloat model.total

    else
        model.entry


subscriptions : Model -> Sub Msg
subscriptions model =
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


mainBg =
    rgb255 57 55 56


digitBg =
    rgb255 0 0 0


equalBg =
    rgb255 250 167 80


operatorBg =
    rgb255 81 56 44


displayBg =
    rgb255 207 208 201


fontColor =
    rgb255 255 255 255


button : String -> Element.Color -> Msg -> Element Msg
button label bg msg =
    Input.button [ width <| Element.px 50, height <| Element.px 50, Border.rounded 25, Background.color bg, Font.color fontColor, Font.center ] { label = text label, onPress = Just msg }


view : Model -> Html Msg
view model =
    Element.layout [ padding 10, Background.color mainBg ]
        (column [ spacing 10 ]
            [ row [ width fill, padding 10, Background.color displayBg ] [ el [ alignRight ] (text (toDisplayValue model)) ]
            , row
                [ spacing 10 ]
                [ button "7" digitBg (PressedDigit 7)
                , button "8" digitBg (PressedDigit 8)
                , button "9" digitBg (PressedDigit 9)
                , button "÷" operatorBg (PressedOperator Divide)
                ]
            , row
                [ spacing 10 ]
                [ button "4" digitBg (PressedDigit 4)
                , button "5" digitBg (PressedDigit 5)
                , button "6" digitBg (PressedDigit 6)
                , button "×" operatorBg (PressedOperator Multiply)
                ]
            , row
                [ spacing 10 ]
                [ button "1" digitBg (PressedDigit 1)
                , button "2" digitBg (PressedDigit 2)
                , button "3" digitBg (PressedDigit 3)
                , button "−" operatorBg (PressedOperator Minus)
                ]
            , row
                [ spacing 10 ]
                [ button "0" digitBg (PressedDigit 0)
                , button "." operatorBg PressedFloating
                , button "=" equalBg (PressedOperator Equal)
                , button "+" operatorBg (PressedOperator Plus)
                ]
            , row
                [ spacing 10 ]
                [ button "C" operatorBg Clear ]
            ]
        )
