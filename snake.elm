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

main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Point = { x: Float, y: Float }
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

stepAngle = 90

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            let
                goal = model.direction % 360
                    |> toFloat
                    |> deg2rad
                    |> asVector
                    |> multiply 5
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


move keyCode model =
    case fromCode keyCode of
        Left ->
            ({ model | direction = model.direction + stepAngle }
            , Cmd.none)
        Right ->
            ({ model | direction = model.direction - stepAngle }
            , Cmd.none)
        _ ->
            (model, Cmd.none)

add: Point -> Point -> Point
add p1 p2 =
    { x = p1.x + p2.x
    , y = p1.y + p2.y
    }

distance p1 p2 =
    sqrt((p1.x - p2.x)^2 + (p1.y - p2.y)^2)

deg2rad degrees =
    degrees * 2 * pi / 360

asVector angle =
    { x = cos angle, y = sin angle}

multiply factor point =
    { x = point.x * factor, y = point.y * factor }

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

type Key =
    Left |
    Right |
    Other

fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        37 ->
            Left
        39 ->
            Right
        _ ->
            Other

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
        ("top", toString position.x ++ "px"),
        ("left", toString position.y ++ "px")
        ]] []
