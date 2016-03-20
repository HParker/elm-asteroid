module Physics where

type alias Space a =
  { a | height : Float, width : Float }

type alias PObject a =
  { a | x : Float, y : Float }

type alias Delta a =
  { a | dx : Float, dy : Float }

type alias Directional a =
  { a | angle : Float }

move : PObject (Delta a) -> PObject (Delta a)
move ({x, y, dx, dy} as obj) =
  { obj |
      x = x + dx,
      y = y + dy
  }

overflow : Space b -> PObject a -> PObject a
overflow {height, width} obj =
  let
    maxHeight = height/2
    maxWidth = width/2
    newX = if abs obj.x > maxWidth then
             -obj.x
           else
             obj.x
    newY = if abs obj.y > maxHeight then
             -obj.y
           else
             obj.y

  in
    { obj |
        x = newX,
        y = newY
    }


speedX : Float -> Float -> Float
speedX angle speed = cos(angle) * speed


speedY : Float -> Float -> Float
speedY angle speed = sin(angle) * speed


updateSpeed : Float -> Directional (Delta a) -> Directional (Delta a)
updateSpeed speed ({angle} as obj) =
    { obj |
        dx = obj.dx + speedX angle speed,
        dy = obj.dy + speedY angle speed
    }


applyFriction : Float -> Delta a -> Delta a
applyFriction friction obj =
  { obj |
      dx = obj.dx * friction,
      dy = obj.dy * friction
  }


turn : Float -> Directional a -> Directional a
turn rotation ({angle} as obj) =
  { obj |
      angle = angle + rotation
  }
