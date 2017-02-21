import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event

type alias Model =
  { input : String
  , shift : Bool
  }

model : Model
model =
  { input = ""
  , shift = False
  }

type Msg
  = Escape
  | Backspace
  | Tab
  | Enter
  | Shift
  | Space
  | Input String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Escape -> { model | input = "" }
    Backspace -> { model | input = String.slice 0 -1 model.input }
    Tab -> { model | input = model.input ++ "    " }
    Enter -> model
    Shift -> { model | shift = not model.shift }
    Space -> { model | input = model.input ++ " " }
    Input input -> { model | input = model.input ++ input }

prependKey : Char -> List (Html Msg) -> List (Html Msg)
prependKey char =
  let
    label = String.fromChar char
  in
    (::) (key [] label (Input label))

shifted : String -> Bool -> String -> String
shifted keys shift shiftedKeys =
  case shift of
    False -> keys
    True -> shiftedKeys

keys : String -> List (Html Msg)
keys keys =
  String.foldr prependKey [] keys

keyboard : Bool -> Html Msg
keyboard shift =
  Html.div
    [ Attr.style [ ("text-align", "center") ] ]
    [ Html.div
      []
      ( [ key [] "ESC" Escape ] ++ keys (shifted "1234567890-=" shift "!@#$%^&*()_+") ++ [ key [] "<-" Backspace ] )
    , Html.div
      []
      ( [ key [] "TAB" Tab ] ++ keys (shifted "qwertyuiop[]" shift "QWERTYUIOP{}") ++ [ key [] "ENTER" Enter ] )
    , Html.div
      []
      ( keys (shifted "asdfghjkl;\'\\" shift "ASDFGHJKL:\"|") )
    , Html.div
      []
      ( [ key [] "SHIFT" Shift ] ++ keys (shifted "`zxcvbnm,./" shift "~ZXCVBNM<>?") ++ [ key [] "SHIFT" Shift ] )
    , Html.div
      []
      ( [ key [ Attr.style [ ("width", "30em") ] ] "SPACE" Space ] )
    ]

key : List (Html.Attribute Msg) -> String -> Msg -> Html Msg
key attrs label effect =
  Html.button
    ( [ Attr.style [ ("min-width", "4em"), ("height", "3em") ], Event.onClick effect ] ++ attrs )
    [ Html.text label ]

view : Model -> Html Msg
view model =
  Html.div
    []
    [ Html.article
      []
      [ Html.input [ Attr.value model.input ] []
      ]
    , Html.aside
      []
      [ keyboard model.shift
      ]
    ]

main = Html.beginnerProgram { model = model , view = view , update = update }
