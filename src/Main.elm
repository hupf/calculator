module Main exposing (Msg(..), main, toDisplayValue, update, view)

import Browser
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type Operator
    = Plus
    | Minus
    | Multiply
    | Divide
    | Equal


type alias Model =
    { total : Float, operator : Maybe Operator, entry : Maybe Float }


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Msg
    = EnterDigit Int
    | EnterOperator Operator
    | Clear


init : Model
init =
    { total = 0
    , operator = Nothing
    , entry = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnterDigit digit ->
            { model | entry = Just (valueOrZero model.entry * 10 + toFloat digit) }

        EnterOperator operator ->
            { model | total = calculate model, operator = Just operator, entry = Nothing }

        Clear ->
            init


valueOrZero : Maybe Float -> Float
valueOrZero value =
    case value of
        Just x ->
            x

        Nothing ->
            0


calculate : Model -> Float
calculate model =
    case model.operator of
        Just Plus ->
            model.total + valueOrZero model.entry

        Just Minus ->
            model.total - valueOrZero model.entry

        Just Multiply ->
            model.total * valueOrZero model.entry

        Just Divide ->
            model.total / valueOrZero model.entry

        Just Equal ->
            model.total

        Nothing ->
            valueOrZero model.entry


toDisplayValue : Model -> String
toDisplayValue model =
    case model.entry of
        Just entry ->
            String.fromFloat entry

        Nothing ->
            String.fromFloat model.total


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
                [ button "7" digitBg (EnterDigit 7)
                , button "8" digitBg (EnterDigit 8)
                , button "9" digitBg (EnterDigit 9)
                , button "÷" operatorBg (EnterOperator Divide)
                ]
            , row
                [ spacing 10 ]
                [ button "4" digitBg (EnterDigit 4)
                , button "5" digitBg (EnterDigit 5)
                , button "6" digitBg (EnterDigit 6)
                , button "×" operatorBg (EnterOperator Multiply)
                ]
            , row
                [ spacing 10 ]
                [ button "1" digitBg (EnterDigit 1)
                , button "2" digitBg (EnterDigit 2)
                , button "3" digitBg (EnterDigit 3)
                , button "-" operatorBg (EnterOperator Minus)
                ]
            , row
                [ spacing 10 ]
                [ button "0" digitBg (EnterDigit 0)
                , button "." operatorBg (EnterDigit 0)
                , button "=" equalBg (EnterOperator Equal)
                , button "+" operatorBg (EnterOperator Plus)
                ]
            , row
                [ spacing 10 ]
                [ button "C" operatorBg Clear ]
            ]
        )
