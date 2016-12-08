module Vector exposing (..)

type alias Point = { x: Float, y: Float }

add: Point -> Point -> Point
add p1 p2 =
    { x = p1.x + p2.x
    , y = p1.y + p2.y
    }

distance p1 p2 =
    sqrt((p1.x - p2.x)^2 + (p1.y - p2.y)^2)

length p =
    distance p { x = 0, y = 0 }

deg2rad degrees =
    degrees * 2 * pi / 360

rad2deg rad =
    rad * 360 / (2 * pi)

angleAsVector angle =
    { x = cos angle, y = sin angle}

vectorAsAngle p =
    angle p { x = 1, y = 0 }
        |> rad2deg

scalarProd: Point -> Point -> Float
scalarProd p1 p2 =
    p1.x * p2.x + p1.y * p2.y

angle: Point -> Point -> Float
angle p1 p2 =
    acos (scalarProd p1 p2)



multiply factor point =
    { x = point.x * factor, y = point.y * factor }
