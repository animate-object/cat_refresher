module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { response : Response
    , previousResults : List String
    }


type Response
    = Failure
    | Loading
    | Success String



-- init defines a nullary function returning a Model and a Command<Message>


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Loading [], getRandomCatGif )



-- UPDATE


type Msg
    = MorePlease
    | GotGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Model Loading model.previousResults, getRandomCatGif )

        GotGif result ->
            case result of
                Ok url ->
                    ( Model (Success url) (url :: model.previousResults), Cmd.none )

                Err _ ->
                    ( Model Failure model.previousResults, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "height" "100vh"
        , style "width" "100vw"
        , style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ h2
            [ style "background-color" "lightgreen"
            , style "margin" "0"
            , style "padding" "1em"
            ]
            [ text "Random Cat" ]
        , div
            [ style "display" "flex"
            , style "flex-grow" "1"
            ]
            [ div
                [ style "width" "50%"
                , style "background-color" "lightblue"
                , style "height" "100%"
                ]
                [ viewGif model ]
            , div
                [ style "width" "50%"
                , style "background-color" "pink"
                , style "height" "100%"
                ]
                [ previousUrls model ]
            ]
        ]


viewGif : Model -> Html Msg
viewGif model =
    case model.response of
        Failure ->
            div []
                [ text "I could not load a random cat for some reasonl"
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success url ->
            div
                [ style "display" "flex"
                , style "flex-direction" "column"
                ]
                [ div
                    [ style "display" "flex"
                    ]
                    [ button
                        [ onClick MorePlease
                        , style "display" "block"
                        , style "height" "10rem"
                        , style "font-size" "2em"
                        , style "width" "100%"
                        ]
                        [ text "More Please!" ]
                    ]
                , img
                    [ src url
                    , style "min-width" "100%"
                    , style "flex-grow" "1"
                    ]
                    []
                ]



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat", expect = Http.expectJson GotGif gifDecoder }


gifDecoder : Decoder String
gifDecoder =
    field "data" (field "image_url" string)


previousUrls : Model -> Html Msg
previousUrls model =
    ol [] (List.map listItem model.previousResults)


listItem : String -> Html Msg
listItem content =
    li [] [ Html.a [ href content, target "_" ] [ text content ] ]
