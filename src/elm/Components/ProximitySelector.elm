module Components.ProximitySelector exposing (view, Parameters)

import Html exposing (Html, div, input, label, text)
import Html.Attributes as A exposing (id, type_, class, for, min, max, step, disabled)
import Html.Events as E exposing (onInput, onCheck)

type alias Parameters =
  { disabled: Bool
  , checked: Bool
  , radius: Float
  }

view : (Bool -> a) -> (String -> a) -> Parameters -> Html a
view onLocationChange onRadiusChange { disabled, checked, radius } =
  let
    (locationDisabled, locationClass) =
      if disabled then (True, "disabled")
      else (False, "")
    (radiusDisabled, radiusClass) =
      if (disabled || (not checked)) then (True, "disabled")
      else (False, "")
  in
    div [ A.class ("proximity-selector ") ]
      [ div [ class "input-group" ]
        [ div [ A.class "input-group" ]
          [ label
            [ A.for "userLocation"
            , A.class locationClass
            ] [ text "Use current location" ]
          , input
            [ A.type_ "checkbox"
            , A.id "userLocation"
            , A.disabled locationDisabled
            , E.onCheck onLocationChange
            ] []
          ]
        , div [ class ("input-group") ]
          [ label
            [ A.for "locationRadius"
            , A.class radiusClass
            ] [ text "Radius" ]
          ,  input
            [ A.type_ "range"
            , A.id "locationRadius"
            , A.min "0"
            , A.max "100"
            , A.step "10"
            , A.disabled locationDisabled
            , A.value (toString radius)
            , E.onInput onRadiusChange
            ] []
          ]
        ]
      ]
