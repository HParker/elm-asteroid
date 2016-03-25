import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Physics
import Random

randSeed : Int
randSeed = 1234

type alias Board = {
    height : Float
  , width : Float
  }


type alias Bullet = {
    x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , expiration : Int
  }


type alias Asteroid = {
    x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , angle : Float
  , size : Float
  }


type alias Ship = {
    x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , angle : Float
  }


type alias Game = {
    board : Board
  , ship : Ship
  , bullets : List Bullet
  , asteroids : List Asteroid
  , seed : Random.Seed
  }


type alias Input = {
    vertical : Int
  , horizontal : Int
  , fire : Bool
  , delta : Time
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


stepAsteroid : Board -> Asteroid -> Asteroid
stepAsteroid board asteroid =
  asteroid
    |> Physics.overflow board
    |> Physics.move
    |> Physics.turn 0.05


colided : List Bullet -> Asteroid -> Bool
colided bullets asteroid =
  List.any (Physics.inside asteroid) bullets

notColided : List Bullet -> Asteroid -> Bool
notColided bullets asteroid =
  not (colided bullets asteroid)

destroyAsteroids : Game -> Game
destroyAsteroids ({bullets, asteroids} as game) =
  { game |
      asteroids = stepAsteroids game.board (List.filter (notColided bullets) asteroids)
  }


generateAsteroid : Game -> (Asteroid, Random.Seed)
generateAsteroid game =
  let
    seed1 = game.seed
    (x, seed2) = Debug.watch "randx" (Random.generate (Random.float (-game.board.width/2) (game.board.width/2)) seed1)
    (y, seed3) = Debug.watch "randy" (Random.generate (Random.float (-game.board.height/2) (game.board.height/2)) seed2)
    (dx, seed4) = Debug.watch "randdx" (Random.generate (Random.float -3 3) seed3)
    (dy, seed5) = Debug.watch "randdy" (Random.generate (Random.float -3 3) seed4)
  in
    ((asteroid x y dx dy), seed5)


generateAsteroids : Game -> Game
generateAsteroids game =
  let
    (asteroid, seed) = generateAsteroid game
    newAsteroids =
      if List.length game.asteroids < 4 then
        asteroid :: game.asteroids
      else
        game.asteroids
  in
    { game |
        asteroids = newAsteroids,
        seed = seed
    }


asteroidGeneration : Game -> Game
asteroidGeneration game =
  destroyAsteroids (generateAsteroids game)

stepAsteroids : Board -> List Asteroid -> List Asteroid
stepAsteroids board asteroids =
      List.map (stepAsteroid board) asteroids

step : Input -> Game -> Game
step input game =
  let
    newGame = asteroidGeneration game
  in
    { newGame |
        ship = stepShip input game,
        bullets = stepBullets input game
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


asteroidShapes : List Asteroid -> List Form
asteroidShapes asteroids =
  List.map asteroidShape asteroids


asteroidShape : Asteroid -> Form
asteroidShape asteroid =
  ngon 5 asteroid.size
    |> outlined (solid white)
    |> move (asteroid.x, asteroid.y)
    |> rotate (asteroid.angle)

bulletShape : Bullet -> Form
bulletShape bullet =
  oval 3 3
    |> filled green
    |> move (bullet.x, bullet.y)


bulletShapes : List Bullet -> List Form
bulletShapes bullets =
  List.map bulletShape bullets


view : (Int, Int) -> Game -> Element
view (w,h) ({board, ship, bullets, asteroids} as game) =
  let
    elements =
      [
       boardShape board,
         shipShape ship
      ]
      `List.append` bulletShapes bullets
      `List.append` asteroidShapes asteroids
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


asteroid : Float -> Float -> Float -> Float -> Asteroid
asteroid x y dx dy =
  Asteroid x y dx dy 0 20

asteroids : List Asteroid
asteroids = []

startGame : Game
startGame = Game skyscape startShip bullets asteroids (Random.initialSeed randSeed)

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
