import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Tuple
import Time exposing (Time)
import Keyboard exposing (downs, KeyCode)
import Debug exposing (log)
import Char
import Random
import Vector exposing (Point, add, distance, deg2rad, angleAsVector, multiply, length)

main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
    { apple: Point,
      snake: Point,
      direction: Int
    }

init : (Model, Cmd Msg)
init =
    ( {
        apple = { x=10, y=30 },
        snake = { x=100, y=30 },
        direction = 0
    }, Cmd.none)

-- UPDATE
type Msg =
    Tick Time |
    KeyDown KeyCode |
    MoveApple Point

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            let
                goal = log "goal" (model.direction % 360
                    |> toFloat
                    |> deg2rad
                    |> angleAsVector
                    |> multiply 0.5)
                cmd =
                    if
                        collide model.snake model.apple
                    then
                        Random.generate MoveApple randomPoint
                    else
                        Cmd.none
            in
                ( { model |
                    snake = add model.snake goal
                  }
                , cmd)

        KeyDown keyCode ->
            move keyCode model

        MoveApple to ->
            ( { model | apple = to }, Cmd.none)

collide : Point -> Point -> Bool
collide p1 p2 =
    distance p1 p2 < 20


move: Int -> Model -> (Model, Cmd Msg)
move keyCode model =
    case fromCode keyCode of
        Nothing ->
            (model, Cmd.none)
        Just direction ->
            case log "length" (length (add direction (angleAsVector (toFloat model.direction)))) of
                0 ->
                    (model, Cmd.none)
                _ ->
                    ({ model | direction = log "direction" (round (Vector.vectorAsAngle direction)) }
                    , Cmd.none)

randomPoint: Random.Generator Point
randomPoint =
    Random.map (\pair ->
        { x = Tuple.first pair
        , y = Tuple.second pair
        }) randomPair

randomPair =
    Random.pair (Random.float 1 300) (Random.float 1 300)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (30 * Time.millisecond) Tick
        , downs KeyDown
        ]

fromCode : Int -> Maybe Point
fromCode keyCode =
    case keyCode of
        37 -> Just { x=-1, y= 0 }
        38 -> Just { x= 0, y= 1 }
        39 -> Just { x= 1, y= 0 }
        40 -> Just { x= 0, y=-1 }
        _  -> Nothing

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ circle model.apple "red"
        , circle model.snake "blue"
        ]

circle: Point -> String -> Html Msg
circle position color =
    div [ style [
        ("position", "absolute"),
        ("background-color", color),
        ("transform", "translate(-50%, -50%)"),
        ("border-radius", "10px"),
        ("height", "20px"),
        ("width", "20px"),
        ("left", toString position.x ++ "px"),
        ("bottom", toString position.y ++ "px")
        ]] []
