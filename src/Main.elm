import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row 
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button

main =
  Browser.sandbox { init = init, update = update, view = view }



type alias Model =
    { todo : String
    , todos : List String
    , success :Bool
    }


init : Model
init =
    {todo ="", todos =[],success =False}  

--update


type Msg
    =AddTodo     
    | RemoveItem String
    |TodoText String
    | RemoveAll     


update : Msg -> Model -> Model
update msg model =
    case msg of
       RemoveAll ->
         { model | todos = [] ,success=False}

       AddTodo ->
         { model | todos = model.todo :: model.todos ,success=True} 

       TodoText s ->
           {model| todo=s}

       RemoveItem text ->
         { model | todos = List.filter (\x -> x /= text) model.todos,success=False }


--view


view : Model -> Html Msg
view model =
     Grid.container []
      [CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        ,Grid.row [ Row.centerXs]  
          [Grid.col[Col.lg2] [],     
         Grid.col [Col.lg8] [ h1 [ ] [ text "TodoList For Beginners" ]
        , input [ value model.todo, onInput TodoText  ] []
        , button [ onClick AddTodo, class "btn btn-primary" ] [ text "Submit" ]
        , button [ onClick RemoveAll, class "btn btn-danger" ] [ text "Remove All" ]
        ,viewValidation model
        , todoList model.todos
        ]  , Grid.col [Col.lg2  ] [ ]
        ]
    ]


todoItem : String -> Html Msg
todoItem todo =
    li [ class "list-group-item" ] [ text todo, button [ onClick (RemoveItem todo), class "btn btn-info" ] [ text "x" ] ]


todoList : List String -> Html Msg
todoList todos =
    let
        child =
            List.map todoItem todos
    in
        ul [ class "list-group" ] child


viewValidation : Model -> Html msg
viewValidation model =
  if model.success then
    div [ style "color" "green" ] [ text "OK" ]
  else
   text " " 
