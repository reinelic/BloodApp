port module Learning exposing (Model, Msg , init , update , view)

import Browser
import Html exposing (..)

import Html.Events exposing ( onClick , onInput)
import Html.Attributes  exposing (class , id)
import Http
import Json.Decode  as Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required , requiredAt)
import File exposing (File)
import LeafletMap


type alias Model  = { 
                      newText : String , 
                      user :  List User 
                      ,status : Status
                      ,errorMessage :String
                      ,activity:String
                      
                      }

type alias DataOptions = {
                            url: String    
                         }

port setData : DataOptions -> Cmd msg

port activityChange: (String -> msg) -> Sub msg



mapElement attributes children =
    node "map-user" attributes children

type alias User =   {
                    countries : String 
                    ,cities :List String
                    ,health : 
                        List {
                                cds : { gitega : List String}
                                ,hospitals : { gitega : List String}
                        }
                    } 
                    

initialModel : Model
initialModel =  { 
                    newText ="Something" 
                    ,user = []
                    ,status= Loading
                    ,errorMessage = ""
                    ,activity = ""
                }


type Status = Loading | Success  | Error 


userDecoder : Decoder User
userDecoder = 
    Decode.succeed User
        |> required "countries" string
        |> required "cities"  (list string)
        |> required "health" (list healthDecoder)



type alias HealthV = { gitega : List String}

type alias Health = {   cds : HealthV
                        ,hospitals: HealthV
                    }

healthDecoder : Decoder Health
healthDecoder = 
        Decode.succeed Health
            |> required "cds" healthVDecoder
            |> required "hospitals" healthVDecoder

healthVDecoder: Decoder HealthV
healthVDecoder = 
    Decode.succeed HealthV
        |> required "gitega" (list string)





type Msg = SendHttpRequest | DataReceived (Result Http.Error (List User)) | Clicked | GotActivity String





view : Model -> Html Msg
view model =
     div [ class "container" ][
        
         div [] [ text model.errorMessage]
         ,  div []  (List.map viewUser model.user )
         ,mapElement [] []
         
        ,div [] [ text " the body"]
        ,viewError model
        ,viewName model
        ,div [ onClick  Clicked] [ text "Sending Data to javascript"]
        ,div [] [ text model.activity]
        
        
     ]

viewUser : User -> Html Msg
viewUser user = 
            
               div [] [ 
                h1 [] [text (user.countries )]
                , div [] (
                    List.map viewCities user.cities
                )
                , div [](
                    List.map viewHealth user.health
                )
                ] 


viewHealth health = 
        div []  [text "running"]




viewCities : String -> Html Msg
viewCities city =  
   

    div [] [ 
        span [][ text "The city is : "]
        ,span [][ text city]
    ]                



getData : Cmd Msg
getData =
    Http.get {
        url="https://api.npoint.io/597e9c1a37aac59bd975/data"
        ,expect = Http.expectJson DataReceived (list userDecoder)
    }

update : Msg -> Model -> (Model , Cmd Msg)

update msg model =

    case msg of
        SendHttpRequest ->
            ( model , getData)
            
        
        DataReceived response  ->
            
            case response of 
                
                Ok val
                            ->( { model | status = Success , user = val}, Cmd.none)

                Err httpError

                            -> ({model | status = Error ,errorMessage = buildErrorMessage (httpError)} , Cmd.none)

        Clicked ->
             (model , setData {url = " this the data"})

        GotActivity activity->
              ({model | activity = activity} , Cmd.none)     

buildErrorMessage : Http.Error -> String
buildErrorMessage  error =
    case error of
        Http.BadUrl message ->
                            message
        Http.Timeout ->
                            "Server is taking long to respond"

        Http.NetworkError ->
                            " Unable to reach the server"

        Http.BadStatus statusCode ->
                            "Request Failed with status code: " ++ String.fromInt statusCode
        Http.BadBody message ->
                            message


    

viewName : Model -> Html Msg
viewName model =
    h1 [] [ text "placeholder"]

viewError : Model ->  Html Msg
viewError model = 
    h1 [] [ text 
    (case model.status of
        Error  -> " oops something went wrong"

        Success -> "Success"

        Loading -> "Still Loading"
    )
      ]    


main: Program String Model Msg
main = Browser.element{
    init = init
    ,view = view
    ,update = update
    ,subscriptions = subscriptions
 }


init : String -> (Model, Cmd Msg)
init act=
            let 
                activity = "this is the first activity"
            in 
            ({initialModel|activity=act}, getData)


subscriptions: Model -> Sub Msg
subscriptions model = 
        activityChange GotActivity




 
-- main: Program () Model Msg
-- main =--     Browser.sandbox { init = initialModel
--                     , view = view
--                     , update = update
                    
--                     }

