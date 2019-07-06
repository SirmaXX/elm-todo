

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)



main =
  Browser.sandbox { init = init, update = update, view = view }



type alias Model =
    { todo : String
    , todos : List String
    }


init : Model
init =
    {todo ="", todos =[]}  

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
         { model | todos = [] }

       AddTodo ->
         { model | todos = model.todo :: model.todos }
       TodoText s ->
           {model| todo=s}

       RemoveItem text ->
         { model | todos = List.filter (\x -> x /= text) model.todos }


--view


view : Model -> Html Msg
view model =
        div []
        [
          input [ value model.todo, onInput TodoText  ] []
        , button [ onClick AddTodo, class "btn btn-primary" ] [ text "Submit" ]
        , button [ onClick RemoveAll, class "btn btn-danger" ] [ text "Remove All" ]
        , todoList model.todos
        ]

todoItem : String -> Html Msg
todoItem todo =
    li [ class "list-group-item" ] [ text todo, button [ onClick (RemoveItem todo), class "btn btn-info" ] [ text "x" ] ]
--sorun burada 1 texti görüntüleyemiyorum

todoList : List String -> Html Msg
todoList todos =
    let
        child =
            List.map todoItem todos
    in
        ul [ class "list-group" ] child

