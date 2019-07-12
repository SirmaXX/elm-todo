import Browser
import Http
import Html exposing (map,text,Html,div,p,ul,button,input,h1,li  )
import Html.Attributes exposing (value,type_ ,checked,class,style)
import Html.Events exposing (onClick,onInput)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row 
import Bootstrap.Grid.Col as Col
import Bootstrap.Alert as Alert


--MAİN

main =
    Browser.document
        { init = init
        , update = update
        , subscriptions  = always Sub.none
        , view = view
       }




type alias Model =
    { todos : List Todo
     ,alertVisibility : Alert.Visibility
     ,entry:String
     ,popup:Int
     ,uid : Int
     ,ip :String
    }

type alias Todo =
    {   id :Int
       ,todo :String
       ,completed:Bool
         }


newTodo : Int -> String -> Todo
newTodo id todo =
  { id = id
  , todo = todo
  , completed = False
  }



--INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ({alertVisibility = Alert.closed,entry="",popup =10 ,todos =[],uid=0,ip =""} ,Cmd.none)
  
   

--UPDATE


type Msg
    =AddTodo     
    | RemoveItem Int
    |TodoText String
    | RemoveAll
    |AlertMsg Alert.Visibility 
    |Check Int Bool   
    | ReceivedIpFromServer (Result Http.Error String)

update : Msg -> Model ->(Model,Cmd Msg)
update msg model =
    case msg of
       RemoveAll ->
        ( { model | todos = [] ,popup=0,alertVisibility = Alert.shown}, Cmd.none )

       AddTodo ->
        ( { model | todos =model.todos ++ [newTodo model.uid  model.entry  ], entry="" ,popup=1,alertVisibility = Alert.shown,uid=model.uid+1} , Cmd.none )

       TodoText text ->
          ({model| entry =text }, Cmd.none )

       RemoveItem id ->
           ({ model |  todos = List.filter (\t-> t.id /= id) model.todos,popup=2,alertVisibility = Alert.shown }, Cmd.none )
        
       AlertMsg visibility ->
            ({ model | alertVisibility = visibility }, Cmd.none )

       Check id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed =isCompleted }
                    else
                         t 
            in
              ({ model | todos = List.map updateEntry model.todos ,popup=3,alertVisibility = Alert.shown }, Cmd.none )
        

       ReceivedIpFromServer (Ok ip) ->
            ( { model | ip = ip }, Cmd.none )

       ReceivedIpFromServer (Err _) ->
            ( { model | ip = "an error occured" }, Cmd.none )






type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

--VİEW

view : Model -> Browser.Document Msg
view model = 
 {  title = "Elm Todo Application"
   , body =
   [  Grid.container []
      [CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        ,Grid.row [ Row.centerXs]  
          [Grid.col[Col.lg2] [],     
         Grid.col [Col.lg8] [ h1 [ ] [ text "TodoList For Beginners" ]
        ,viewValidation model
        , input [ value model.entry, onInput TodoText  ] []
        , button [ onClick AddTodo, class "btn btn-primary" ] [ text "Submit" ]
        , button [ onClick RemoveAll, class "btn btn-danger" ] [ text "Remove All" ]
         ,todoList model
        ]  , Grid.col [Col.lg2  ] [ ]
        ]
    ]
   ]
 }


todoItem : Todo -> Html Msg
todoItem todoitem =
    li [ class "list-group-item" ] [ 
       if todoitem.completed == True then
         div [ style "color" "green" ] [ text todoitem.todo ]
       else
       div [ ] [ text todoitem.todo], button [ onClick (RemoveItem   todoitem.id), class "btn btn-info" ] [ text "x" ],
  input[ class "toggle-all",type_ "checkbox", checked todoitem.completed, onClick (Check todoitem.id (not todoitem.completed)) ][] ]
               


todoList : Model -> Html Msg
todoList todos=
    let
        child =
            List.map todoItem todos.todos
    in
        ul [ class "list-group" ] child



-- alert sections

viewValidation : Model -> Html Msg
viewValidation model =
   case model.popup of
     0 ->
       deleteall model
     1 ->
       succeed model
     2 ->
       deleteitem model
     3 ->
       changetocomp model

     _ ->
       text " "



succeed : Model -> Html Msg
succeed model =
    Alert.config
        |> Alert.success
        |> Alert.dismissable AlertMsg
        |> Alert.children
            [ 
            text "Yapılacak iş listeye eklendi"
            ]
        |> Alert.view model.alertVisibility



deleteitem : Model -> Html Msg
deleteitem model =
    Alert.config
        |> Alert.warning
        |> Alert.dismissable AlertMsg
        |> Alert.children
            [ 
            text "Listeden bir iş silindi"
            ]
        |> Alert.view model.alertVisibility



deleteall : Model -> Html Msg
deleteall model =
    Alert.config
        |> Alert.danger
        |> Alert.dismissable AlertMsg
        |> Alert.children
            [ 
            text "Bütün Yapılacaklar silindi"
            ]
        |> Alert.view model.alertVisibility



changetocomp : Model -> Html Msg
changetocomp model =
    Alert.config
        |> Alert.primary
        |> Alert.dismissable AlertMsg
        |> Alert.children
            [ 
            text "İş tamamlandı"
            ]
        |> Alert.view model.alertVisibility






