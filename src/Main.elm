module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode


type alias Model =
    { total : Float, operator : Maybe Operator, entry : String }


type Msg
    = PressedDigit Digit
    | PressedOperator Operator
    | PressedFloating
    | PressedKey Key
    | Clear


type alias Digit =
    Int


type Operator
    = Plus
    | Minus
    | Multiply
    | Divide
    | Equal


type Key
    = Character Char
    | Control String


type KeyValue
    = DigitKey Digit
    | OperatorKey Operator
    | FloatingKey
    | ClearKey


type BtnStyle
    = BtnDigit
    | BtnEqual
    | BtnOperator


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
    div [ class "calculator" ]
        [ div [ class "display" ] [ div [] [ text (toDisplayValue model) ] ]
        , btn "7" BtnDigit (PressedDigit 7)
        , btn "8" BtnDigit (PressedDigit 8)
        , btn "9" BtnDigit (PressedDigit 9)
        , btn "÷" BtnOperator (PressedOperator Divide)
        , btn "4" BtnDigit (PressedDigit 4)
        , btn "5" BtnDigit (PressedDigit 5)
        , btn "6" BtnDigit (PressedDigit 6)
        , btn "×" BtnOperator (PressedOperator Multiply)
        , btn "1" BtnDigit (PressedDigit 1)
        , btn "2" BtnDigit (PressedDigit 2)
        , btn "3" BtnDigit (PressedDigit 3)
        , btn "−" BtnOperator (PressedOperator Minus)
        , btn "0" BtnDigit (PressedDigit 0)
        , btn "." BtnOperator PressedFloating
        , btn "=" BtnEqual (PressedOperator Equal)
        , btn "+" BtnOperator (PressedOperator Plus)
        , btn "C" BtnOperator Clear
        ]


toDisplayValue : Model -> String
toDisplayValue model =
    if model.entry == "" then
        String.fromFloat model.total

    else
        model.entry


btn : String -> BtnStyle -> Msg -> Html Msg
btn label style msg =
    let
        bg =
            case style of
                BtnDigit ->
                    "digit"

                BtnEqual ->
                    "equal"

                BtnOperator ->
                    "operator"
    in
    button [ class "btn", class ("btn-" ++ bg), onClick msg ] [ text label ]
