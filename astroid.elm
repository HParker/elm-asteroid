import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Physics
import Random

type alias Board = {
    height : Float,
    width : Float
  }


type alias Bullet = {
    x : Float,
    y : Float,
    dx : Float,
    dy : Float,
    expiration : Int
  }

type alias Astroid = {
    x : Float,
    y : Float,
    dx : Float,
    dy : Float,
    angle : Float,
    size : Float
  }


type alias Ship =
  {
    x : Float,
    y : Float,
    dx : Float,
    dy : Float,
    angle : Float
  }


type alias Game =
  {
    board : Board,
    ship : Ship,
    bullets : List Bullet,
    astroids : List Astroid
  }


type alias Input =
  { vertical : Int,
    horizontal : Int,
    fire : Bool,
    delta : Time
  }


ship : Float -> Float -> Ship
ship x y = Ship x y 0 0 0


stepShip : Input -> Game -> Ship
stepShip input game =
  game.ship
    |> Physics.updateSpeed (toFloat input.vertical)
    |> Physics.overflow game.board
    |> Physics.move
    |> Physics.applyFriction 0.95
    |> Physics.turn (-(toFloat input.horizontal) * 0.1)


canFire : List Bullet -> Bool
canFire bullets =
  case List.head bullets of
    Just x -> x.expiration < 5
    Nothing -> True

age : { a | expiration : Int } -> { a | expiration : Int }
age model =
  { model | expiration = model.expiration - 1 }

stepBullet : Bullet -> Bullet
stepBullet bullet =
  bullet
    |> Physics.move
    |> age


liveBullets : List Bullet -> List Bullet
liveBullets bullets = List.filter (\b -> b.expiration > 0) bullets

stepBullets : Input -> Game -> List Bullet
stepBullets input ({board, ship, bullets} as game) =
  let
    dx = Physics.speedX ship.angle 15
    dy = Physics.speedY ship.angle 15
    newBullets =
      if input.fire && canFire bullets then
        (bullet ship.x ship.y dx dy) :: bullets
      else
        game.bullets
  in
    List.map stepBullet (liveBullets newBullets)


stepAstroid : Astroid -> Astroid
stepAstroid astroid =
  { astroid |
      angle = astroid.angle + 0.05
  }

-- liveAstroids : Game -> List Astroid
-- liveAstroids ({bullets, astroids} as game) =


stepAstroids : Game -> List Astroid
stepAstroids ({bullets, astroids} as game) =
  List.map stepAstroid astroids

step : Input -> Game -> Game
step input game =
  { game |
      ship = stepShip input game,
      bullets = stepBullets input game,
      astroids = stepAstroids game
  }


-- VIEW

boardShape : Board -> Form
boardShape board =
  rect board.width board.height
    |> filled black


shipShape : Ship -> Form
shipShape ship =
  group
    [
     ngon 3 15
      |> outlined (solid white),
     rect 2 15
      |> filled red
      |> move (-10, 0)
    ]
    |> move (ship.x, ship.y)
    |> rotate (ship.angle)


astroidShapes : List Astroid -> List Form
astroidShapes astroids =
  List.map astroidShape astroids


astroidShape : Astroid -> Form
astroidShape astroid =
  ngon 5 astroid.size
    |> outlined (solid white)
    |> move (astroid.x, astroid.y)
    |> rotate (astroid.angle)

bulletShape : Bullet -> Form
bulletShape bullet =
  oval 3 3
    |> filled green
    |> move (bullet.x, bullet.y)


bulletShapes : List Bullet -> List Form
bulletShapes bullets =
  List.map bulletShape bullets


view : (Int, Int) -> Game -> Element
view (w,h) ({board, ship, bullets, astroids} as game) =
  let
    elements =
      [
       boardShape board,
         shipShape ship
      ]
      `List.append` bulletShapes bullets
      `List.append` astroidShapes astroids
  in
    container w h middle <| collage (round board.width) (round board.height) elements

-- STARTS

startShip : Ship
startShip = ship 0 0

skyscape : Board
skyscape = Board 400 600

bullet : Float -> Float -> Float -> Float -> Bullet
bullet x y dx dy = Bullet x y dx dy 15

bullets : List Bullet
bullets = []


astroid : Astroid
astroid = Astroid
          35
          35
          0
          0
          0
          20

astroids : List Astroid
astroids = [astroid]

startGame : Game
startGame = Game skyscape startShip bullets astroids

-- INPUT

delta : Signal Float
delta = Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
        Signal.map4 Input
                (Signal.map .y Keyboard.wasd)
                (Signal.map .x Keyboard.wasd)
                Keyboard.space
                delta

gameState : Signal Game
gameState = Signal.foldp step startGame input

main : Signal Element
main = Signal.map2 view Window.dimensions gameState
