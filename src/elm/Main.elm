import Html
import Html exposing (Html, text, div, input, label, section, h1, p, a, header, h2, h3)
import Html.Attributes as A exposing (href, id, for, class, placeholder, autofocus, classList, src, name, type_, title, checked, value, disabled, min, max)
import Html.Events as E exposing (onCheck, onInput)
import String


main : Program Never Model Msg
main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
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

type alias Model =
  { currentSearch : CurrentSearch
  , searchResults: List SearchResult
  }


model : Model
model =
  { currentSearch =
    { name = ""
    , location = Nothing
    , radius = 0.0
    }
  , searchResults = []
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

update : Msg -> Model -> Model
update message model =
  case message of
    SetText text ->
      { model | currentSearch = updateName text model.currentSearch }
    ToggleLocalization checked ->
      let search = if checked then Nothing else Nothing
      in
        { model | currentSearch = updateLocation search model.currentSearch }
    SetLocation latitude longitude ->
      let location = Just <| Location latitude longitude
      in
        { model | currentSearch = updateLocation location model.currentSearch }
    SetRadius radius ->
        { model | currentSearch = updateRadius radius model.currentSearch }


type Msg
  = SetText String
  | ToggleLocalization Bool
  | SetLocation Float Float
  | SetRadius Float


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
        [ h2 [] [ text "Elm with Gulp and SASS configured" ]
        , h3 [] [ text ( "Version " ) ]
        , p []
          [ text "Have a look at the "
          , a [ A.href "https://github.com/pietro909/elm-quickstart/blob/master/README.md" ] [ text "readme" ]
          , text " for more information."
          ]
        ]
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
