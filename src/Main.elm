module Main exposing (main)
import Learning as Learn
import Browser exposing (Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Url.Parser as Parser exposing ((</>) , Parser , s , string)


type alias Model = {
                     page : Page 
                     ,key: Nav.Key
                    
                     }

type Page = HomePage | LearnPage Learn.Model | NotFound

type Route  = Home | Learning

type Msg = 
            ClickedLink Browser.UrlRequest 
            | ChangedUrl Url
            | GotLearnMsg Learn.Msg


view: Model -> Document Msg 
view  model = 
        let 
            content =      
                case model.page of 
                    LearnPage learn  -> 
                        Learn.view learn
                            |> Html.map GotLearnMsg

                    HomePage -> div [class "container"] [ text " This is the Homepage"]

                    NotFound ->  div [class "container"] [ text " Not Found !!!"]

        in 
            {
                title = "Learning Elm App"
                , body = [
                            viewHeader
                            ,content
                        ]
            }


viewHeader  = 
            let 
                logo =  span [] [ text " Blood App"]

                links = ul [class "nav"] 
                        [                      
                         li [ class "nav-item" ] 
                            [  a [ href "/" 
                              , class "nav-link"] [  text "Home"]]
                         , li [ class "nav-item"  ] 
                            [ a [ href "/learn"
                                 , class "nav-link"]
                                 [ text " Learning"]]
                         ]

            in
                nav [ class " navbar navbar-expanded navbar-light bg-light"] 
                    [
                        div [ class "container-fluid"][
                            a [ class "navbar-brand"] [logo]
                            ,div []
                            [
                            links
    
                        ]
                    ]
                    ]
                
           


update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
        
        case msg of 
 
            ClickedLink urlRequest  ->
                case  urlRequest of
                    Browser.External href ->
                        (model , Nav.load href)

                    Browser.Internal url ->
                        ( model , Nav.pushUrl model.key (Url.toString url))
                
            ChangedUrl url ->
                urlToPage url model 

            GotLearnMsg learnMsg ->
                 case model.page  of
                 
                    LearnPage learnModel
                        -> toLearn model (Learn.update learnMsg learnModel)

                    _ -> ( model ,Cmd.none)



       
parser : Parser (Route -> a) a
parser = Parser.oneOf
        [
        Parser.map Home Parser.top
        ,Parser.map Learning ( Parser.s"learn")
        ]

-- changing the page and map cmd msgs

toLearn : Model -> (Learn.Model , Cmd Learn.Msg) -> (Model , Cmd Msg)
toLearn model ( learn , cmd) =
        ( { model | page = LearnPage learn} ,  Cmd.map GotLearnMsg cmd)


-- parsing  the url and depending on the url assignin the page
-- initialing the model of the  pages and  return cmd msg this to have access 

urlToPage : Url ->  Model -> (Model , Cmd Msg)
urlToPage url model  = 
    case Parser.parse parser url of
            Just Learning ->

                toLearn model (Learn.init "this is a test")

            Just Home ->
                ({ model | page = HomePage } , Cmd.none)

            Nothing ->
                 ({ model | page = NotFound } , Cmd.none)

            




init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key   =
         urlToPage  url { page = HomePage  , key = key}
    




main = Browser.application {
    init  = init 
    ,onUrlRequest = \ _ -> Debug.todo " handle all url "
    , onUrlChange = \ _ -> Debug.todo " handle only back and internal route"
    ,view = view
    ,update = update
    ,subscriptions = \_ -> Sub.none
 }            