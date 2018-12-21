import Control.Category ((>>>))
import Data.Function    ((&))
import Gam              (InputMotion(..), MouseButton(..), P, pattern P,
                         Picture, RGBA(..), Sprite(..), SpriteSheet(..), V,
                         Window(..))
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
  { placing :: Maybe Planet
  , planets :: [Planet]
  } deriving stock (Show)

data Planet
  = Planet
  { position :: P
  , velocity :: V
  , radius :: Float
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
      model
        { planets = stepPlanets dt (planets model)
        , placing =
            (\planet ->
              planet { radius = radius planet + 0.03 * dt }) <$> placing model
        }

    Click ButtonLeft Pressed point ->
      model
        { placing =
            Just Planet
              { position = point
              , velocity = 0
              , radius = 8
              }
        }

    Click ButtonLeft Released point ->
      case placing model of
        Nothing ->
          model

        Just planet ->
          model
            { planets =
                planet
                  { velocity =
                      V.scale 0.001 (P.subtract (position planet) point)
                  }
                : planets model
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
    , pictures =
        maybe id ((:) . renderPlanet True) (placing model)
          (map (renderPlanet False) (planets model))
    , scale = (1, 1)
    , size = (640, 480)
    , title = "Planets"
    }

renderPlanet :: Bool -> Planet -> Picture
renderPlanet placing (Planet { position, radius }) =
  sprite
    & Picture.translate (P.asV (position - P radius radius))
    & (let n = radius / 8 in Picture.scale (n, n))
    & if placing then Picture.alpha 0.5 else id
  where
    sprite =
      Picture.sprite Sprite
        { sheet = SpriteSheet
            { file = "gam-examples/planet.png"
            , spriteSize = (16, 16)
            }
        , indices = [(0, 0)]
        }
