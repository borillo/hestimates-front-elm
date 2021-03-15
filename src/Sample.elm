module Sample exposing (main)

import Browser

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Http exposing (..)
import Json.Decode exposing (Decoder, at, decodeString, int, list)

import List exposing (length, map)

type alias Model =
  { inputValue : Maybe Int,
    estimates: List Int,
    isButtonDisabled: Bool,
    error: Maybe String
  }
  
checkResult : (Result Http.Error (List Int)) -> Msg
checkResult result = case result of
    Err _ -> Error "Error loading estimates"
    Ok data -> EstimatesReceived data

estimateDecoder : Decoder (List Int)
estimateDecoder = Json.Decode.list (at [ "id" ] int)

initModel : () -> ( Model, Cmd Msg )
initModel _ = (
  { inputValue = Maybe.Nothing
    , estimates = []
    , isButtonDisabled = True
    , error = Nothing
  }, Http.get
      { url = "http://localhost:8080/estimationss"
      , expect = Http.expectJson checkResult (estimateDecoder) 
      } )

main : Program () Model Msg
main =
  Browser.element { 
    init = initModel, 
    update = update, 
    view = view, 
    subscriptions = subscriptions 
  }

type Msg = 
  AddEstimate Int | 
  ChangeId String |
  EstimatesReceived (List Int) | 
  Error String

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddEstimate id ->
      ({ model | estimates = model.estimates ++ [id], inputValue = Nothing }, Cmd.none)
    ChangeId id -> 
      let iv = case id of
                 "" -> Nothing
                 _ -> case String.toInt id of
                        Nothing -> model.inputValue
                        a -> a
      in 
        ({ model | inputValue = iv, isButtonDisabled = iv == Nothing }, Cmd.none)
    EstimatesReceived data -> 
        ({ model | estimates = data }, Cmd.none)
    Error message -> 
        ({ model | error = Just message }, Cmd.none)

intToDiv : Int -> Html Msg
intToDiv id = div [] [text (String.fromInt id)]

view : Model -> Html Msg
view model =
  div [] ((case model.error of 
            Nothing -> []
            Just msg -> [div [] [text msg]])  ++
  [ 
    Html.form [onSubmit (AddEstimate (Maybe.withDefault 0 model.inputValue))]
    [ input [
        value (Maybe.withDefault "" (Maybe.map String.fromInt model.inputValue)), 
        size 10,
        maxlength 10,
        onInput ChangeId 
      ] [],
      input [         
        type_ "submit",
        disabled model.isButtonDisabled 
      ] [ text "Add estimate" ],
      div [] (List.map intToDiv model.estimates)
    ]
  ])