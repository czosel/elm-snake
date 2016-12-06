module Vector exposing (..)

type alias Point = { x: Float, y: Float }

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
