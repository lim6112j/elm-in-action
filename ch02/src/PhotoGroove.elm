module PhotoGroove exposing (main)
import Browser
import Html exposing (Html, div, h1, img, text, button, input, label, h3)
import Html.Attributes exposing(..)
import Html.Events exposing(onClick)
import Array exposing(Array)
type alias Photo =
    { url : String}
type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize}
type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe

type ThumbnailSize
    = Small
    | Medium
    | Large
view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise me"]
        , h3 [] [ text "Thumbnail Size:"]
        , div [ id "choose-size"]
            (List.map viewSizeChooser [Small, Medium, Large])
        , div [ id "thumbnails" , class (sizeToString model.chosenSize)]
            (List.map
                 (viewThumbnail model.selectedUrl)
                 model.photos
            )
        , img
            [class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]
viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
       img
       [
        src (urlPrefix ++ thumb.url)
        , classList [("selected", selectedUrl == thumb.url)]
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
update : Msg -> Model -> Model
update msg model =
    case msg of
    ClickedPhoto url ->
        { model | selectedUrl = url }
    ClickedSurpriseMe ->
        { model | selectedUrl = "2.jpeg"}
    ClickedSize size ->
        { model | chosenSize = size}

initModel: Model
initModel =
    {
    photos =
        [ {url = "1.jpeg"}
        , {url = "2.jpeg"}
        , {url = "3.jpeg"}
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Large
    }
photoArray: Array Photo
photoArray = Array.fromList initModel.photos

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"
getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url
        Nothing ->
            ""
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update}
