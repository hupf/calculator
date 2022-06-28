module Main exposing (Msg(..), main, update, view)

-- import Html exposing (Html, button, div, text)

import Browser
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- view model =
--     div []
--         [ button [ onClick Decrement ] [ text "-" ]
--         , div [] [ text (String.fromInt model) ]
--         , button [ onClick Increment ] [ text "+" ]
--         ]


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


button label bg =
    Input.button [ width <| Element.px 50, height <| Element.px 50, Border.rounded 25, Background.color bg, Font.color fontColor, Font.center ] { label = text label, onPress = Nothing }


view model =
    Element.layout [ padding 10 ]
        (column [ spacing 10 ]
            [ row [ width fill, padding 10, Background.color displayBg ] [ el [] (text " ") ]
            , row
                [ spacing 10 ]
                [ button "7" digitBg
                , button "8" digitBg
                , button "9" digitBg
                , button "÷" operatorBg
                ]
            , row
                [ spacing 10 ]
                [ button "4" digitBg
                , button "5" digitBg
                , button "6" digitBg
                , button "×" operatorBg
                ]
            , row
                [ spacing 10 ]
                [ button "1" digitBg
                , button "2" digitBg
                , button "3" digitBg
                , button "-" operatorBg
                ]
            , row
                [ spacing 10 ]
                [ button "0" digitBg
                , button "." operatorBg
                , button "=" equalBg
                , button "+" operatorBg
                ]
            ]
        )
