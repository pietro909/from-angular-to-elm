import Html
import Html exposing (Html, text, div, input, label, section, h1, p, a, header, h2, h3)
import Html.Attributes as A exposing (href, id, for, class, placeholder, autofocus, classList, src, name, type_, title, checked, value, disabled, min, max)
import Html.Events as E exposing (onCheck, onInput)
import String
import Geolocation
import Task
import Tuple
import String.Interpolate as StringInt
import Http
import Json.Decode exposing (string, int, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional)


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
  , query: String
  , error: Maybe String
  , rawResponse: Maybe String
  }


initialModel : Model
initialModel =
  { currentSearch =
    { name = ""
    , location = Nothing
    , radius = 0.0
    }
  , searchResults = []
  , query = ""
  , error = Nothing
  , rawResponse = Nothing
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
location_template = "&location={0},{1}&locationradius={2}km"

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

type alias LOF =
  { items: List SearchResult }

aliasToList obj =
  obj

decodeVideos : Decoder (List SearchResult)
decodeVideos =
  decode aliasToList
    |> required "items" (list buildVideo)

searchVideo : CurrentSearch -> Cmd Msg
searchVideo search =
  let
    request = Http.get (buildQueryString search) decodeVideos
  in
    Http.send LoadVideos request

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    SetText text ->
      let
        command = searchVideo model.currentSearch
      in
        (
          { model
          | currentSearch = updateName text model.currentSearch
          }
        , command
        )
    ToggleLocalization checked ->
      if checked then
        let
          resultHandler result =
            case result of
              Ok location -> 
                SetLocation location.latitude location.longitude
              Err error -> 
                ToggleLocalization False
          command = Task.attempt resultHandler Geolocation.now
        in (model, command)
      else
        let
          command = searchVideo model.currentSearch
        in
          (
            { model
            | currentSearch = updateLocation Nothing model.currentSearch
            }
          , command
          )  
    SetLocation latitude longitude ->
      let
        location = Just <| Location latitude longitude
        command = searchVideo model.currentSearch
      in
        (
          { model
          | currentSearch = updateLocation location model.currentSearch
          }
        , command
        )
    SetRadius radius ->
      let
        command = searchVideo model.currentSearch
      in
        (
          { model
          | currentSearch = updateRadius radius model.currentSearch 
          }
        , command
        )
    LoadVideos (Ok response) ->
      (
        { model
        | rawResponse = Just (toString response)
        , searchResults = response}
      , Cmd.none
      )
    LoadVideos (Err errString) ->
      ({ model | error = Just (toString errString) }, Cmd.none)


type Msg
  = SetText String
  | ToggleLocalization Bool
  | SetLocation Float Float
  | SetRadius Float
  | LoadVideos (Result Http.Error SearchResults)


onRadius : String -> Msg
onRadius input =
  case (String.toFloat input) of
    Ok radius ->
      SetRadius radius
    Err error ->
      SetRadius 0.0


view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ header [ A.class "row" ]
      [ div [ A.class "col-xs-12 col-sm-12 col-md-12 menu" ]
        [ h1 [] [ text "Elm Quickstart" ] ]
      ]
    , section [ A.class "row" ]
      [ div [ A.class "col-xs-12 col-sm-10 col-md-8 col-lg-6" ]
        [ h2 [] [ text "to elm" ] ]
      , div [ class "col-xs-12 col-sm-10 col-md-8 col-lg-6" ]
        [ input [ A.type_ "text", A.class "form-control", A.placeholder "Search", A.autofocus True, E.onInput SetText ] []
        , div [ A.class "input-group" ]
          [ label [ A.for "userLocation" ] [ text "Use current location" ]
          , input [ A.type_ "checkbox", A.disabled False, E.onCheck ToggleLocalization ] []
          ]
        , div [ class "input-group" ]
          [ input [ A.type_ "range", A.min "1", A.max "100", A.value "50", A.disabled False, E.onInput onRadius ] []
          ]
        ]
      ]
    ]
