module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom exposing (focus)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput)
import Task exposing (attempt)



-- MAIN


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { input : String
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { input = "" }
    , Cmd.none
    )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = Submit
    | Input String
    | InsertLambda
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( { model | input = "" }, Cmd.none )

        Input str ->
            ( { model | input = str }, Cmd.none )

        InsertLambda ->
            ( { model | input = model.input ++ "λ" }, Task.attempt (\_ -> NoOp) (focus "formula-input") )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Lambda Calculus Animator"
    , body =
        [ div []
            [ input [ id "formula-input", value model.input, onInput Input ] []
            , button [ onClick Submit ]
                [ text "Animate" ]
            , div
                []
                [ button [ onClick InsertLambda ] [ text "Insert λ" ]
                ]
            ]
        ]
    }
