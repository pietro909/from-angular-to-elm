module Components.ProximitySelector exposing (view)

import Html exposing (Html, div, input, label, text)
import Html.Attributes as A exposing (id, type_, class, for, min, max, step, disabled)
import Html.Events as E exposing (onInput, onCheck)


view : (Bool -> a) -> (String -> a) -> Bool -> Bool -> Float -> Html a
view onLocationChange onRadiusChange locationIsAvailable locationIsOn radius =
  let
    disabled =
      if locationIsAvailable then ""
      else "disabled"
    checkBoxDisabled =
      if locationIsOn then ""
      else "disabled"
  in
    div [ A.class ("proximity-selector ") ]
      [ div [ class "input-group" ]
        [ div [ A.class "input-group" ]
          [ label
            [ A.for "userLocation"
            , A.class disabled
            ] [ text "Use current location" ]
          , input
            [ A.type_ "checkbox"
            , A.id "userLocation"
            , A.disabled (not locationIsAvailable)
            , E.onCheck onLocationChange
            ] []
          ]
        , div [ class ("input-group") ]
          [ label
            [ A.for "locationRadius"
            , A.class checkBoxDisabled
            ] [ text "Radius" ]
          ,  input
            [ A.type_ "range"
            , A.id "locationRadius"
            , A.min "0"
            , A.max "100"
            , A.step "10"
            , A.disabled (not locationIsOn)
            , A.value (toString radius)
            , E.onInput onRadiusChange
            ] []
          ]
        ]
      ]
