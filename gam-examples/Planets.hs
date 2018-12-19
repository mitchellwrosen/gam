import Control.Category ((>>>))
import Data.Function    ((&))
import Gam              (InputMotion(..), MouseButton(..), P, pattern P,
                         Picture, RGBA(..), SpriteSheet(..), V, Window(..))
import Gam.Main.Tea.Sub (Sub)

import qualified Gam.Main.Tea
import qualified Gam.Main.Tea.Sub as Sub
import qualified Gam.P            as P
import qualified Gam.Picture      as Picture
import qualified Gam.V            as V

import Debug.Trace

main :: IO ()
main =
  Gam.Main.Tea.main
    initialModel
    subscriptions
    (\msg model -> pure (update msg model))
    display

data Model
  = Model
  { placing :: Maybe P
  , planets :: [Planet]
  } deriving stock (Show)

data Planet
  = Planet
  { position :: P
  , velocity :: V
  } deriving stock (Show)

data Msg
  = Tick Float
  | Mouse P
  | Click MouseButton InputMotion P
  deriving stock (Show)

initialModel :: Model
initialModel =
  Model
    { placing = Nothing
    , planets = []
    }

subscriptions :: Model -> Sub Msg
subscriptions _ =
  mconcat
    [ Sub.millis Tick
    , Sub.mouseMotion Mouse
    , Sub.mouseClicks Click
    ]

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Tick dt ->
      model { planets = stepPlanets dt (planets model) }

    Click ButtonLeft Pressed point ->
      model { placing = Just point }

    Click ButtonLeft Released point ->
      case placing model of
        Nothing ->
          model

        Just origin ->
          model
            { planets =
                Planet origin (V.scale 0.001 (P.subtract origin point)) :
                  planets model
            , placing =
                Nothing
            }

    _ ->
      model

stepPlanets :: Float -> [Planet] -> [Planet]
stepPlanets dt =
  filterOutOfBoundsOrColliding >>>
  (\ps -> map (stepPlanet dt ps) ps)
  where
    filterOutOfBoundsOrColliding =
      loop
      where
        loop = \case
          [] -> []
          p:ps ->
            if outOfBounds (position p)
              then loop ps
              else p : loop ps

        outOfBounds (P px py) =
          px < 0 || px > 640 || py < 0 || py > 480

        -- colliding :: Planet -> Planet -> Bool
        -- colliding p1 p2 =
        --   P.distance (position p1) (position p2) <= 1

stepPlanet :: Float -> [Planet] -> Planet -> Planet
stepPlanet dt planets planet =
  planet
    { position =
        P.add (V.scale dt (velocity planet)) (position planet)
    , velocity =
        sum (velocity planet : map f planets)
    }
  where
    f :: Planet -> V
    f other =
      let
        v = P.subtract (position planet) (position other)
        q = V.quadrance v
      in
        if q == 0
          then 0
          else V.scale (min 0.01 (0.1 / q)) v

display :: Model -> Window
display model =
  Window
    { background = RGBA 0 0 0 0
    , pictures = map renderPlanet (planets model)
    , scale = (1, 1)
    , size = (640, 480)
    , title = "Planets"
    }

renderPlanet :: Planet -> Picture
renderPlanet (Planet { position }) =
  Picture.sprite (SpriteSheet "gam-examples/planet.png" (16, 16)) 0
    & Picture.translate (P.asV (position - P 8 8))
