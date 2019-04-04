module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, map2, string)



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
    { response : ApiResponse
    , previousResults : List ImageMeta
    }


type ApiResponse
    = Failure
    | Loading
    | Success ImageMeta


type alias ImageMeta =
    { url : String
    , title : String
    }



-- init defines a nullary function returning a Model and a Command<Message>


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Loading [], getRandomCatGif )



-- UPDATE


type Msg
    = MorePlease
    | GotGif (Result Http.Error ImageMeta)
    | LoadPrevious ImageMeta


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Model Loading model.previousResults, getRandomCatGif )

        GotGif result ->
            case result of
                Ok imageMeta ->
                    ( Model (Success imageMeta) (imageMeta :: model.previousResults), Cmd.none )

                Err _ ->
                    ( Model Failure model.previousResults, Cmd.none )

        LoadPrevious imageMeta ->
            ( Model (Success imageMeta) model.previousResults, Cmd.none )



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
                , style "overflow" "auto"
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

        Success imageMeta ->
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
                    [ src imageMeta.url
                    , style "min-width" "100%"
                    , style "flex-grow" "1"
                    ]
                    []
                ]



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat", expect = Http.expectJson GotGif gifDecoder }


gifDecoder : Decoder ImageMeta
gifDecoder =
    map2 ImageMeta (field "data" (field "image_url" string)) (field "data" (field "title" string))



-- field "data" (field "image_url" string)


previousUrls : Model -> Html Msg
previousUrls model =
    ol [] (List.reverse (List.map listItem model.previousResults))


listItem : ImageMeta -> Html Msg
listItem content =
    li [] [ button [ onClick (LoadPrevious content), target "_" ] [ text content.title ] ]
