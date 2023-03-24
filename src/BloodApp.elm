module BloodApp exposing (main)

import Browser
import Html exposing (..)
import Random
import Html.Events exposing ( onClick , onInput)
import String exposing (..)
import Http

import Json.Decode  as Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required , requiredAt)





type alias BloodStock = {
       groupeA:Quantite
        ,groupeB:Quantite
        ,groupeC:Quantite
    }

type alias Quantite  = {quantite: String }


type alias Stock = {
                
                        id : String 
                        , lieu: String 
                        ,province: String
                        ,stock : BloodStock
                    
                }


type Status  = Loading | Error String | Received






type alias Model= { 
    status: Status
    ,stock : Maybe (List Stock)
    } 


initialModel: Model


initialModel = {
                status = Loading
               , stock =  Nothing
                }

type Msg = GotStock (Result Http.Error ( List Stock))



buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =

        case httpError of
            Http.BadUrl  message ->
                message

            Http.Timeout ->
                "Server is taking long to respond"
            
            Http.NetworkError  ->
                "Unable to reach server"

            Http.BadStatus statusCode ->
                "Request failed with status code: " ++ fromInt statusCode
            
            Http.BadBody message ->
                message




{-- createCnts : Int -> Cnts -> Html Msg

createCnts value cnts  =
 ul [] [
    li [ ] 
    [ 
        text (cnts ++ " ") 
       , span [] [text (fromInt value)] 
       ,button [ onClick Increase] [text "increase"]
       ,button [ onClick Decrease] [text "Decrease"]
       ,button [onClick Random] [text "Suprise Me"]
    ]
 ]
 --}



update : Msg-> Model-> (Model , Cmd Msg) 
update msg model =

    case msg of 

        GotStock result ->
         
            case result of

                Ok response  ->
                     
                         ({ model | status = Received ,stock =  Just response } , Cmd.none)   
                           
                Err httpError  ->

                             ({model | status = Error <|(buildErrorMessage httpError)}, Cmd.none)

                  



    

view : Model-> Html Msg
view model =
    div[] (
        case model.status of

            Loading ->

             [ text "LOADING"]


            Received ->

                case  model.stock of

                    Just response ->

                        List.map showStock response

                    Nothing ->

                        [ text " We did not receive anything "]   
            
            Error err ->

                [ text err] 

            
    )
                
     


showStock : Stock -> Html Msg
 
showStock stock =
     div [][
         h1[][text stock.id]
         ,h1[][text stock.province]
         ,h1[][text stock.lieu]
         ,hr [] []
         
     ]






quantiteDecoder : Decoder Quantite
quantiteDecoder =
    succeed buildQuantity
        |> required "quantite" string

buildQuantity: String -> Quantite
buildQuantity quantite =
    {
        quantite = quantite
    }

bloodStockDecoder: Decoder BloodStock
bloodStockDecoder = 
    succeed buildBlood
        |> required "groupeA" quantiteDecoder
        |> required "groupeB" quantiteDecoder
        |> required "groupeC" quantiteDecoder
         
           


buildBlood :  Quantite ->  Quantite ->Quantite ->BloodStock
buildBlood groupeA groupeB groupeC =
    {
        groupeA = groupeA
        ,groupeB = groupeB
        ,groupeC = groupeC
    }


stockDecoder : Decoder Stock
stockDecoder = 
    succeed buildStock

        |> required "id" string
        |> required "lieu" string
        |> required "stock" bloodStockDecoder
        |> required "province" string


buildStock : String -> String-> BloodStock->String ->Stock
buildStock id lieu stockG province =
            {
                id = id
                ,lieu = lieu
                ,province = province
                ,stock = {
                    groupeA = { quantite=stockG.groupeA.quantite}
                    ,groupeB = { quantite=stockG.groupeB.quantite}
                    ,groupeC = { quantite=stockG.groupeC.quantite}
                }
                
            }




{--



type alias BloodStock = {
       groupeA:Quantite
        ,groupeB:Quantite
        ,groupeC:Quantite
    }

type alias Quantite  = {quantite: String }


type alias Stock = {
                            id : String 
                        , lieu: String 
                        ,province: String
                        ,stock : {
                            groupeA : { quantite : String}
                            ,groupeB : { quantite : String}
                            ,groupeC : { quantite : String}
                        }
                     
                    
                }

--}

initialCmd : Cmd Msg
initialCmd = Http.get {
    url = "https://api.npoint.io/ea1ca62d45f56a0c083b"
    ,expect = Http.expectJson GotStock (list stockDecoder)
 }


 
main: Program () Model Msg
main =
    Browser.element { init = \flags ->(initialModel , initialCmd)
                    , view = view
                    , update = update
                    ,subscriptions = \_ -> Sub.none
                    }

