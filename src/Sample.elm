module Sample exposing (main)

import Browser

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import List exposing (length, map)

type alias Model =
  { inputValue : Maybe Int,
    estimates: List Int,
    isButtonDisabled: Bool
  }
  
initModel : Model
initModel = 
  { inputValue = Maybe.Nothing
    , estimates = []
    , isButtonDisabled = True
  }
main : Program () Model Msg
main =
  Browser.sandbox { init = initModel, update = update, view = view }

type Msg = 
  AddEstimate Int | 
  ChangeId String | 
  DoNothing

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddEstimate id ->
      { model | estimates = model.estimates ++ [id], inputValue = Nothing }
    ChangeId id -> 
      let iv = case id of
                 "" -> Nothing
                 _ -> case String.toInt id of
                        Nothing -> model.inputValue
                        a -> a
      in 
        { model | inputValue = iv, isButtonDisabled = iv == Nothing }
    _ -> model

intToDiv : Int -> Html Msg
intToDiv id = div [] [text (String.fromInt id)]

view : Model -> Html Msg
view model =
  div []
  [ Html.form [onSubmit DoNothing]
    [ input [
        value (Maybe.withDefault "" (Maybe.map String.fromInt model.inputValue)), 
        size 10,
        maxlength 10,
        onInput ChangeId 
      ] [],
      button [ 
        onClick (AddEstimate (Maybe.withDefault 0 model.inputValue)), 
        disabled model.isButtonDisabled 
      ] [ text "Add estimate" ],
      div [] (List.map intToDiv model.estimates)
    ]
  ]