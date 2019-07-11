import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row 
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Card as Card

main =
  Browser.sandbox{ init = init, update = update, view = view }



type alias Model =
    { todos : List Todo
     ,alertVisibility : Alert.Visibility
     ,entry:String
     ,popup:Int
     ,uid : Int
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




init : Model
init =
    {todos =[],entry="",popup =10 ,alertVisibility = Alert.closed,uid=0}  

--update


type Msg
    =AddTodo     
    | RemoveItem Int
    |TodoText String
    | RemoveAll
    |AlertMsg Alert.Visibility 
    |Check Int Bool   


update : Msg -> Model -> Model
update msg model =
    case msg of
       RemoveAll ->
         { model | todos = [] ,popup=0,alertVisibility = Alert.shown}

       AddTodo ->
         { model | todos =model.todos ++ [newTodo model.uid  model.entry  ], entry="" ,popup=1,alertVisibility = Alert.shown,uid=model.uid+1} 

       TodoText text ->
           {model| entry =text }

       RemoveItem id ->
           { model |  todos = List.filter (\t-> t.id /= id) model.todos,popup=2,alertVisibility = Alert.shown }
        
       AlertMsg visibility ->
            { model | alertVisibility = visibility }

       Check id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed =isCompleted }
                    else
                        t
            in
             { model | todos = List.map updateEntry model.todos ,popup=3,alertVisibility = Alert.shown }
            

--view


view : Model -> Html Msg
view model =
     Grid.container []
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




todoItem : Todo -> Html Msg
todoItem todoitem =
    li [ class "list-group-item" ] [ 
       if todoitem.completed == True then
         div [ style "color" "green" ] [ text todoitem.todo ]
       else
       div [ style "color" "red" ] [ text todoitem.todo], button [ onClick (RemoveItem   todoitem.id), class "btn btn-info" ] [ text "x" ],
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
            text "Yapılacak şey listeye eklendi"
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




