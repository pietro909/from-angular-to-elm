import Html
import Html exposing (Html, text, div, input, label, section, h1, p, a, header, h2, h3, img)
import Html.Attributes as A exposing (href, id, for, class, placeholder, autofocus, classList, src, name, type_, title, checked, value, disabled, min, max)
import Html.Events as E exposing (onCheck, onInput)
import String
import Geolocation
import Task
import Tuple
import String.Interpolate as StringInt
import Http
import Json.Encode
import Json.Decode exposing (string, int, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional)
import Maybe.Extra as ME
import Result.Extra as RE

import Components.ProximitySelector as ProximitySelector

minNameLength = 3


main : Program Never Model Msg
main =
  Html.program
    { init = (initialModel, Cmd.none)
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }

type alias Location =
  { latitude: Float
  , longitude: Float
  }

type alias CurrentSearch =
  { name: String
  , location: Maybe Location
  , radius: Float
  }

type alias SearchResult =
  { id: String
  , title: String
  , thumbnailUrl: String
  }

type alias SearchResults = List SearchResult

type alias Model =
  { currentSearch : CurrentSearch
  , searchResults: SearchResults
  , enableLocalization: Bool
  , query: String
  , errors: List Error
  }


initialModel : Model
initialModel =
  { currentSearch = CurrentSearch "" Nothing 0.0
  , searchResults = []
  , enableLocalization = True
  , query = ""
  , errors = [ EmptySearchBox ]
  }

updateName : String -> CurrentSearch -> CurrentSearch
updateName name model =
  { model | name = name }

updateLocation : Maybe Location -> CurrentSearch -> CurrentSearch
updateLocation location model =
  { model | location = location }

updateRadius : Float -> CurrentSearch -> CurrentSearch
updateRadius radius model =
  { model | radius = radius }


-- YOUTUBE STUFF

youtube_api_key = "AIzaSyDOfT_BO81aEZScosfTYMruJobmpjqNeEk"
youtube_api_url = "https://www.googleapis.com/youtube/v3/search"
location_template = "&location={0},{1}&locationRadius={2}km"

isValidSearch : CurrentSearch -> Bool
isValidSearch search = (String.length search.name) > minNameLength

buildQueryString : CurrentSearch -> String
buildQueryString search =
  let
    locationString =
      case search.location of
        Just location ->
          let
            radius = if (search.radius == 0.0) then 50 else search.radius
            params = List.map toString [ location.latitude, location.longitude, radius ]
          in
            StringInt.interpolate location_template params
        Nothing ->
          ""
    params =
      [ locationString
      , "&q=" ++ search.name
      , "&key=" ++ youtube_api_key
      , "&part=snippet"
      , "&type=video"
      , "maxResults=50"
      ]
  in
    (youtube_api_url ++ "?") ++ (List.foldl (++) "" params)


buildVideo : Decoder SearchResult
buildVideo =
  decode SearchResult
    |> requiredAt ["id", "videoId"] string
    |> requiredAt ["snippet", "title"] string
    |> requiredAt ["snippet", "thumbnails", "high", "url"] string

{-- }
type alias LOF =
  { items: List SearchResult }
  --}

aliasToList obj =
  obj

decodeVideos : Decoder (List SearchResult)
decodeVideos =
  decode aliasToList
    |> required "items" (list buildVideo)

searchVideo : CurrentSearch -> Cmd Msg
searchVideo search =
  if (isValidSearch search) then
    Http.get (buildQueryString search) decodeVideos
    |> Http.send LoadVideos
  else
    Cmd.none

setEmptySearchError : String -> List Error -> List Error
setEmptySearchError name errors =
  if ((String.length name) > 3) then
    removeError EmptySearchBox errors
  else
    updateErrors EmptySearchBox errors

setLocationError : Maybe String -> List Error -> List Error
setLocationError maybeError errors =
  case maybeError of
    Just errorMsg ->
      updateErrors (LocationUnavailable errorMsg) errors
    Nothing ->
      removeError (LocationUnavailable "") errors

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    SetText text ->
      let
        nextSearch = updateName text model.currentSearch
        command = searchVideo nextSearch
        errors = setEmptySearchError nextSearch.name model.errors
      in
        ({ model | currentSearch = nextSearch, errors = errors }, command)

    DisableLocalization reason ->
      let errors = setLocationError (Just reason) model.errors 
      in ({ model | errors = errors }, Cmd.none)

    ToggleLocalization True ->
      let
        resultHandler result =
          case result of
            Ok location ->
              SetLocation location.latitude location.longitude
            Err error ->
              DisableLocalization (toString error)
        command = Task.attempt resultHandler Geolocation.now
      in ({ model | enableLocalization = True }, command )

    ToggleLocalization False ->
      let
        nextSearch = updateLocation Nothing model.currentSearch
        command = searchVideo nextSearch
      in
        ({ model | currentSearch = nextSearch, enableLocalization = False }, command)

    SetLocation latitude longitude ->
      let
        location = Just <| Location latitude longitude
        nextSearch = updateLocation location model.currentSearch
        command = searchVideo nextSearch
        errors = setLocationError Nothing model.errors
      in
        ({ model | errors = errors, currentSearch = nextSearch }, command)

    SetRadius radius ->
      let
        nextSearch = updateRadius radius model.currentSearch
        command = searchVideo model.currentSearch
      in
        ({ model | currentSearch = nextSearch }, command)

    LoadVideos (Ok response) ->
      let errors = removeError (SearchError "") model.errors
      in ({ model | errors = errors, searchResults = response }, Cmd.none)

    LoadVideos (Err errString) ->
      let errors = updateErrors (SearchError (toString errString)) model.errors
      in ({ model | errors = errors }, Cmd.none)


type Msg
  = SetText String
  | DisableLocalization String
  | ToggleLocalization Bool
  | SetLocation Float Float
  | SetRadius Float
  | LoadVideos (Result Http.Error SearchResults)

type Error
  = EmptySearchBox
  | LocationUnavailable String
  | SearchError String


isSearchError error =
  case error of
    SearchError _ -> True
    _ -> False

isEmptySearchBox error =
  case error of
    EmptySearchBox -> True
    _ -> False

isLocationUnavailable error =
  case error of
    LocationUnavailable _ -> True
    _ -> False

removeError : Error -> List Error -> List Error
removeError error errors =
  case error of
    EmptySearchBox ->
      List.filter (\e -> not (isEmptySearchBox e)) errors
    LocationUnavailable _ ->
      List.filter (\e -> not (isLocationUnavailable e)) errors
    SearchError _ ->
      List.filter (\e -> not (isSearchError e)) errors

updateErrors : Error -> List Error -> List Error
updateErrors error errors =
  error :: (removeError error errors)

viewError : Error -> Html Msg
viewError error =
  case error of
    EmptySearchBox ->
      div [ class "row col-md-8 alert alert-danger" ]
        [ p [] [ text "Can't search with an empty searchbox" ] ]
    SearchError details ->
      div [ class "row col-md-8 alert alert-danger" ]
        [ p [] [ text details ]]
    LocationUnavailable details ->
      div [ class "row col-md-8 alert alert-warning" ]
        [ p [] [ text details ]]


onRadius : String -> Msg
onRadius input = SetRadius (RE.extract (\_ -> 0.0)  (String.toFloat input))


view : Model -> Html Msg
view model =
  section [ class "col-md-8" ]
    [ h1 [] [ text "Stuff" ]
    , viewInput model
    , div [ class "row col-md-8" ]
      (List.map viewError model.errors)
    , div [ class "row col-md-8" ]
      [ p []
        [ text """
          Try to type something in the searchbox, play with the location and
          with radius: the above state always be consistent and up to date.
        """]
      , p [ class "state" ] [ text (toString model.currentSearch) ]
      , h2 [] [ text "Search results:" ]
      ]
    , div [ class "row col-md-8" ]
      (List.map viewThumbnail model.searchResults)
    ]

viewInput : Model -> Html Msg
viewInput model =
  let
    proxSelParams = 
      ProximitySelector.Parameters 
        (not (model.enableLocalization && (isValidSearch model.currentSearch)))
        (ME.isJust model.currentSearch.location)
        model.currentSearch.radius
  in
    div [ class "row col-md-8" ]
      [ viewSearchBox model
      , viewProximitySelector proxSelParams
      ]

viewSearchBox : Model -> Html Msg
viewSearchBox model =
  div [ class "search-box" ]
    [ input
      [ A.type_ "text"
      , A.class "form-control"
      , A.placeholder "Search"
      , A.autofocus True
      , E.onInput SetText
      ] []
    ]

viewProximitySelector : ProximitySelector.Parameters -> Html Msg
viewProximitySelector =
  ProximitySelector.view ToggleLocalization onRadius


viewThumbnail : SearchResult -> Html Msg
viewThumbnail result =
  div [ class "thumbnail col-sm-6 col-md-4" ]
    [ div [ class "caption" ]
      [ h3 [] [ text result.title ] ]
    , img [ src result.thumbnailUrl ] []
    ]
