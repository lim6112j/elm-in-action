module PhotoGroove exposing (main)
import Browser
import Html exposing (Html, div, h1, img, text, button, input, label, h3, node)
import Html.Attributes as Attr exposing(class, id, src, title, classList)
import Html.Events exposing(onClick)
import Array exposing(Array)
import Random
import Http
import Json.Decode as Decode exposing(Decoder, string, int, succeed)
import Json.Decode.Pipeline exposing(optional, required)
import Json.Encode as Encode
import Html.Attributes exposing (type_)
import Html.Attributes exposing (name)
import Html.Events exposing(on, onClick)

type alias Photo =
    { url : String
    , size : Int
    , title : String
    }
type alias Model =
    {
      status : Status
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }
type Msg
    = ClickedPhoto String
    | GotSelectedIndex Int
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int
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
                 viewLoaded photos url model
             Loading ->
                 []
             Errored errorMessage ->
                 [ text ("Error: " ++ errorMessage)]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos url model =
    [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise me"]
        , div [ class "filters" ]
          [ viewFilter SlidHue "Hue" model.hue
          , viewFilter SlidRipple "Ripple" model.ripple
          , viewFilter SlidNoise "Noise" model.noise
          ]
        , h3 [] [ text "Thumbnail Size:"]
        , div [ id "choose-size"]
            (List.map viewSizeChooser [Small, Medium, Large])
        , div [ id "thumbnails" , class (sizeToString model.chosenSize)]
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
    SlidHue hue ->
        ({model | hue = hue}, Cmd.none)
    SlidRipple ripple ->
        ({model | ripple = ripple}, Cmd.none)
    SlidNoise noise ->
        ({model | noise = noise}, Cmd.none)
initModel: Model
initModel =
    {
      status = Loading
    , chosenSize = Large
    , hue = 5
    , ripple = 5
    , noise = 5
    }
initCmd : Cmd Msg
initCmd =
    Http.get
        {url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Decode.list photoDecoder)
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
        |> Json.Decode.Pipeline.required "size" Decode.int
        |> optional "title" string "(untitled)"

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, initCmd)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none}
rangeSlider : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children
viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude)]
        ]
onSlide : (Int -> msg) -> Html.Attribute msg
onSlide toMsg =
    Decode.at ["detail", "userSlidTo"] int
        |> Decode.map toMsg
        |> on "slide"
