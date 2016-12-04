import Html exposing (Html, Attribute, div, text)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String
import Time exposing (Time)
import Keyboard exposing (downs, KeyCode)
import Debug exposing (log)
import Char

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
    KeyDown KeyCode

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
                apple =
                    if
                        collide model.snake model.apple
                    then
                        { x = 100, y = 200}
                    else
                        model.apple
            in
                ( { model |
                    snake = add model.snake goal,
                    apple = apple
                  }
                , Cmd.none)
        KeyDown keyCode ->
            let
                _ = log "direction" model.direction
            in
                move keyCode model

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
        [ div [ style [
            ("position", "absolute"),
            ("background-color", "red"),
            ("transform", "translate(-50%, -50%)"),
            ("border-radius", "10px"),
            ("height", "20px"),
            ("width", "20px"),
            ("top", toString model.apple.x ++ "px"),
            ("left", toString model.apple.y ++ "px")
            ]] [],
        div [ style [
            ("position", "absolute"),
            ("background-color", "blue"),
            ("transform", "translate(-50%, -50%)"),
            ("border-radius", "10px"),
            ("height", "20px"),
            ("width", "20px"),
            ("top", toString model.snake.x ++ "px"),
            ("left", toString model.snake.y ++ "px")
            ]] []
        ]

