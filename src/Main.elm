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
    , favorites : List ImageMeta
    , viewState : ViewState
    }


type DataPanelState
    = Favorites
    | PreviousResults


type alias ViewState =
    { dataPanel : DataPanelState
    }


type ApiResponse
    = Failure
    | Loading
    | Success ImageMeta


type alias ImageMeta =
    { url : String
    , title : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Loading [] [] (ViewState PreviousResults), getRandomCatGif )



-- UPDATE


type Msg
    = MorePlease
    | GotGif (Result Http.Error ImageMeta)
    | LoadPrevious ImageMeta
    | SaveFavorite ImageMeta
    | UpdateViewState ViewState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( { model | response = Loading }, getRandomCatGif )

        GotGif result ->
            case result of
                Ok imageMeta ->
                    ( { model | response = Success imageMeta, previousResults = imageMeta :: model.previousResults }, Cmd.none )

                Err _ ->
                    ( { model | response = Failure }, Cmd.none )

        LoadPrevious imageMeta ->
            ( { model | response = Success imageMeta }, Cmd.none )

        SaveFavorite imageMeta ->
            ( { model | favorites = imageMeta :: model.favorites }, Cmd.none )

        UpdateViewState newViewState ->
            ( { model | viewState = newViewState }, Cmd.none )



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
                , style "display" "flex"
                , style "flex-direction" "column"
                ]
                [ viewGif model ]
            , div
                [ style "width" "50%"
                , style "background-color" "pink"
                , style "height" "100%"
                , style "overflow" "auto"
                ]
                [ dataPanelViewToggle model
                , dataPanel model
                ]
            ]
        ]


dataPanel : Model -> Html Msg
dataPanel model =
    case model.viewState.dataPanel of
        Favorites ->
            favorites model

        PreviousResults ->
            previousResults model


favorites : Model -> Html Msg
favorites model =
    savedItems model.favorites


dataPanelViewToggle : Model -> Html Msg
dataPanelViewToggle model =
    case model.viewState.dataPanel of
        Favorites ->
            button
                [ style "width" "100%"
                , style "display" "block"
                , style "height" "8rem"
                , style "font-size" "2rem"
                , onClick (UpdateViewState { dataPanel = PreviousResults })
                ]
                [ text "Go To Previous Results" ]

        PreviousResults ->
            button
                [ style "width" "100%"
                , style "display" "block"
                , style "height" "8rem"
                , style "font-size" "2rem"
                , onClick
                    (UpdateViewState { dataPanel = Favorites })
                ]
                [ text "Go To All Time Favorites" ]


viewGif : Model -> Html Msg
viewGif model =
    case model.response of
        Failure ->
            div []
                [ text "I could not load a random cat for some reasonl"
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            div
                [ style "height" "8rem", style "width" "100%", style "height" "10rem" ]
                [ text "Loading. . ." ]

        Success imageMeta ->
            div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "flex-grow" "1"
                ]
                [ button
                    [ onClick MorePlease
                    , style "display" "block"
                    , style "height" "8rem"
                    , style "font-size" "2em"
                    , style "width" "100%"
                    ]
                    [ text "More Please!" ]
                , img
                    [ src imageMeta.url
                    , style "flex-grow" "1"
                    ]
                    []
                , button
                    [ onClick (SaveFavorite imageMeta)
                    , style "display" "block"
                    , style "height" "8rem"
                    , style "font-size" "2em"
                    , style "width" "100%"
                    ]
                    [ text "Save to Favorites" ]
                ]


previousResults : Model -> Html Msg
previousResults model =
    savedItems model.previousResults


savedItems : List ImageMeta -> Html Msg
savedItems images =
    ol [] (List.reverse (List.map listItem images))


listItem : ImageMeta -> Html Msg
listItem content =
    li [] [ button [ onClick (LoadPrevious content), target "_" ] [ text content.title ] ]



-- HTTP


getRandomCatGif : Cmd Msg
getRandomCatGif =
    Http.get { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat", expect = Http.expectJson GotGif gifDecoder }


gifDecoder : Decoder ImageMeta
gifDecoder =
    map2 ImageMeta (field "data" (field "image_url" string)) (field "data" (field "title" string))
