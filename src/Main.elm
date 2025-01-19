module Main exposing (main)

import Browser
import Html exposing (Html, div, h2, table, thead, tbody, tr, th, td, text)
import Html.Attributes exposing (class)
import Http exposing (Header,Error(..))
import Json.Decode exposing (Decoder, field, list, map6, string, succeed, fail, andThen)
import Time exposing (Posix, toHour, toMinute, millisToPosix, utc)
import Date exposing (Date, fromIsoString)

-- MODEL

type alias Model =
    { departures : List Departure
    , error : Maybe String
    }

type alias Departure =
    { direction : String
    , departureDate : Date
    , departureTime : Time.Posix
    , tripShortName : String
    , physicalMode : String
    , commercialMode : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] Nothing , fetchDepartures )


-- MESSAGES

type Msg
    = DeparturesReceived (Result Http.Error (List Departure))


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeparturesReceived (Ok departures) ->
            ( { model | departures = departures, error = Nothing}, Cmd.none )

        DeparturesReceived (Err httpError) ->
            ( { model | error = Just (httpErrorToString httpError)}, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "container mx-auto p-4 font-sans" ]
        [ h2 [ class "text-2xl font-bold mb-4" ] [ text "Prochains Départs - Gare Montparnasse" ]
        , 
            case model.error of
                Just errMsg ->
                    div [ class "text-red-500" ] [ text ("Erreur: " ++ errMsg) ]

                Nothing ->
                    if List.isEmpty model.departures then
                        div [ class "text-gray-500" ] [ text "Aucun départ disponible." ]
                    else
                        let
                            maybeDate = 
                                model.departures
                                    |> List.head
                                    |> Maybe.map .departureDate
                        in
                        div []
                            [ table [ class "min-w-full bg-white" ]
                                [ thead [ class "bg-gray-200" ]
                                    [ tr []
                                        [ th [ class "py-2 px-4 border-b" ] [ text "Train" ]
                                        , th [ class "py-2 px-4 border-b" ] [ text "Direction" ]
                                        , th [ class "py-2 px-4 border-b" ] [ text "Heure de Départ" ]
                                        , th [ class "py-2 px-4 border-b" ] [ text "Mode Physique" ]
                                        , th [ class "py-2 px-4 border-b" ] [ text "Mode Commercial" ]
                                        ]
                                    ]
                                , tbody []
                                    (List.map viewDeparture model.departures)
                                ]
                            , 
                                case maybeDate of
                                    Just date ->
                                        let
                                            formattedDate = Date.format "dd MMMM yyyy" date
                                        in
                                        div [ class "mt-4 text-right text-gray-600" ]
                                            [ text ("Date: " ++ formattedDate) ]

                                    Nothing ->
                                        text ""
                            ]
        ]

viewDeparture : Departure -> Html Msg
viewDeparture departure =
    tr [ class "hover:bg-gray-100" ]
        [ td [ class "py-2 px-4 border-b" ] [ text departure.tripShortName ]
        , td [ class "py-2 px-4 border-b" ] [ text departure.direction ]
        , td [ class "py-2 px-4 border-b" ]
            [ text (formatTime departure.departureTime) ]
        , td [ class "py-2 px-4 border-b" ] [ text departure.physicalMode ]
        , td [ class "py-2 px-4 border-b" ] [ text departure.commercialMode ]
        ]

-- Helper function to format time as "HH:MM"
formatTime : Posix -> String
formatTime posix =
    let
        hour = (toHour utc posix)
        minute = (toMinute utc posix)
    in
    String.fromInt hour ++ ":" ++ (if minute < 10 then "0" ++ String.fromInt minute else String.fromInt minute)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- HTTP REQUEST

api_url : String
api_url =
    "https://api.sncf.com/v1/coverage/sncf/stop_areas/stop_area:SNCF:87391003/departures?datetime=20250119T132131"

api_key_header : Header
api_key_header =
    Http.header "Authorization" "API_KEY_HERE"

fetchDepartures : Cmd Msg
fetchDepartures = 
            Http.request
                {
                method = "GET"
                , url = api_url
                , body = Http.emptyBody
                , expect = Http.expectJson DeparturesReceived listDepartureDecoder
                , headers = [ api_key_header ]
                , timeout = Nothing
                , tracker = Nothing
                }


-- JSON DECODERS

listDepartureDecoder : Decoder (List Departure)
listDepartureDecoder =
    field "departures" (list departureDecoder)


departureDecoder : Decoder Departure
departureDecoder =
    map6 Departure
        (field "display_informations"
            (field "direction" string)
        )
        (field "stop_date_time"
            (field "departure_date_time" fromIsoStringDateDecoder)
        )
        (field "stop_date_time"
            (field "departure_date_time" timeDecoder)
        )
        (field "display_informations"
            (field "trip_short_name" string)
        )
        (field "display_informations"
            (field "physical_mode" string)
        )
        (field "display_informations"
            (field "commercial_mode" string)
        )


fromIsoStringDateDecoder : Decoder Date
fromIsoStringDateDecoder =
    string
      |> andThen
        (\isoString ->
          case String.split "T" isoString of
            [ date, _ ] ->
              case fromIsoString date of
                Ok data ->
                  succeed data

                Err _ ->
                  fail "Invalid date format"

            _ ->
              fail "Expected a 'T' separator."
        )

timeDecoder : Decoder Posix
timeDecoder =
    string
        |> andThen
            (\isoString ->
                case String.split "T" isoString of
                    [ _, time ] ->
                        let
                            hour = Maybe.withDefault 0 (String.toInt (String.left 2 time))
                            minute = Maybe.withDefault 0 (String.toInt (String.slice 2 4 time))
                            second = Maybe.withDefault 0 (String.toInt (String.right 2 time))
                            totalMillis =
                                (hour * 60 * 60 * 1000)
                                    + (minute * 60 * 1000)
                                    + (second * 1000)
                        in
                        succeed (millisToPosix totalMillis)

                    _ ->
                        fail "Invalid ISO format: Expected a 'T' separator."
            )


-- ERROR HANDLING

httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Timeout ->
            "La requête a expiré. Veuillez réessayer."

        NetworkError ->
            "Erreur réseau. Veuillez vérifier votre connexion internet."

        BadUrl url ->
            "URL invalide: " ++ url

        BadStatus responseBody ->
            "Erreur du serveur: " ++ String.fromInt responseBody

        BadBody bodyDecoderError ->
            "Erreur de décodage du corps de la réponse: " ++  bodyDecoderError


-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
