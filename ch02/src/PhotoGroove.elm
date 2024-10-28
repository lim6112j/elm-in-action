module PhotoGroove exposing (main)
import Browser
import Html exposing (Html, div, h1, img, text, button, input, label, h3)
import Html.Attributes exposing(..)
import Html.Events exposing(onClick)
import Array exposing(Array)
import Random
import Http
import Json.Decode exposing(Decoder, string, int, succeed)
import Json.Decode.Pipeline exposing(optional, required)
type alias Photo =
    { url : String
    , size : Int
    , title : String
    }
type alias Model =
    {
      status : Status
    , chosenSize : ThumbnailSize}
type Msg
    = ClickedPhoto String
    | GotSelectedIndex Int
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))

type ThumbnailSize
    = Small
    | Medium
    | Large
type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String

view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
             Loaded photos url ->
                 viewLoaded photos url model.chosenSize
             Loading ->
                 []
             Errored errorMessage ->
                 [ text ("Error: " ++ errorMessage)]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos url chosenSize =
    [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise me"]
        , h3 [] [ text "Thumbnail Size:"]
        , div [ id "choose-size"]
            (List.map viewSizeChooser [Small, Medium, Large])
        , div [ id "thumbnails" , class (sizeToString chosenSize)]
            (List.map
                 (viewThumbnail url)
                 photos
            )
        , img
            [class "large"
            , src (urlPrefix ++ "large/" ++ url)
            ]
            []
    ]
viewThumbnail : String -> Photo -> Html Msg
viewThumbnail url thumb =
       img
       [
        src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [("selected", url == thumb.url)]
        , onClick (ClickedPhoto thumb.url)
       ]
       []
viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [
         input [ type_ "radio", name "size", onClick (ClickedSize size)] []
        , text (sizeToString size)
        ]
sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"
        Medium ->
            "med"
        Large ->
            "large"
selectedUrl : String -> Status -> Status
selectedUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        Loading ->
            status
        Errored _ ->
            status
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
    ClickedPhoto url ->
        ({ model | status = selectedUrl url model.status }, Cmd.none)
    ClickedSurpriseMe ->
        case model.status of
            Loaded (firstPhoto :: otherPhotos) _ ->
                Random.uniform firstPhoto otherPhotos
                    |> Random.generate GotRandomPhoto
                    |> Tuple.pair model
            Loaded [] _ ->
                (model, Cmd.none)
            Loading ->
                (model, Cmd.none)
            Errored _ ->
                (model, Cmd.none)
    ClickedSize size ->
        ({ model | chosenSize = size}, Cmd.none)
    GotSelectedIndex index ->
        ({ model | status = selectedUrl (getPhotoUrl index model.status) model.status }, Cmd.none)
    GotRandomPhoto photo ->
        ({ model | status = selectedUrl photo.url model.status}, Cmd.none)
    GotPhotos (Ok photos) ->
        case photos of
            (first :: _) ->
                ({model | status = Loaded photos first.url }, Cmd.none)
            [] ->
                ({model | status = Errored "0 photos found"}, Cmd.none)
    GotPhotos (Err _) ->
        (model, Cmd.none)
initModel: Model
initModel =
    {
      status = Loading
    , chosenSize = Large
    }
initCmd : Cmd Msg
initCmd =
    Http.get
        {url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)
        }

photoArray : Status -> Array Photo
photoArray status =
    case status of
        Loaded photos _ ->
            Array.fromList photos
        Loading ->
            Array.empty
        Errored _ ->
            Array.empty
getPhotoUrl : Int -> Status -> String
getPhotoUrl index status =
    case Array.get index (photoArray status) of
        Just photo ->
            photo.url
        Nothing ->
            ""
urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> Json.Decode.Pipeline.required "url" string
        |> Json.Decode.Pipeline.required "size" int
        |> optional "title" string "(untitled)"

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, initCmd)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none}
