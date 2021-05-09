module Main exposing (main)

import Browser
import Http
import Json.Decode exposing (Decoder, field, string)

import Html exposing (Html, br, text, div, p, select, option)
import Html.Events exposing (onInput)
import Html.Attributes exposing (class, value)

type Model =
    Loading
    | Fail
    | Success String

type Msg =
    InitReq String
    | RequestPrice (Result Http.Error String)

pairsDecoder: String -> Decoder String
pairsDecoder currency = 
    field "bpi" <| field currency <| field "rate" string

getPrice: String -> Cmd Msg
getPrice val = 
    Http.get 
        {
            url = "https://api.coindesk.com/v1/bpi/currentprice.json",
            expect = Http.expectJson RequestPrice (pairsDecoder val)
        }

init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getPrice "USD")
  
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    InitReq currency ->
      (Loading, getPrice currency)

    RequestPrice result ->
      case result of
        Ok val ->
          (Success val, Cmd.none)

        Err _ ->
          (Fail, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

customView : Model -> Html Msg
customView model =
  case model of
    Fail ->
      text " An error occured while fetching the data. Please reaload the page or try again later"

    Loading ->
      text "Loading..."

    Success price ->
      text <| "1 Bitoin = " ++ price ++ " in"

view: Model -> Html Msg
view model =
    div [class "layout"][
        div [class "navbar"][ 
            p [] [text "BitPrices"] ],

        p [][
            
            customView model,
            br [] [], 
            select [onInput (\currency -> InitReq currency)][
                option [value "USD"][text "USD"],
                option [value "EUR"][text "EUR"],
                option [value "GBP"][text "GBP"]
                ]
        ]
    ]

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }