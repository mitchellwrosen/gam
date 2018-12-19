import Data.Function    ((&))
import Gam              (InputMotion(..), MouseButton(..), P, pattern P,
                         Picture, RGBA(..), SpriteSheet(..), V, Window(..))
import Gam.Main.Tea.Sub (Sub)

import qualified Gam.Main.Tea
import qualified Gam.Main.Tea.Sub as Sub
import qualified Gam.P            as P
import qualified Gam.Picture      as Picture


main :: IO ()
main =
  Gam.Main.Tea.main
    initialModel
    subscriptions
    (\msg model -> pure (update msg model))
    display

data Model
  = Model
  { planets :: [Planet]
  } deriving stock (Show)

data Planet
  = Planet
  { position :: P
  , velocity :: V
  } deriving stock (Show)

data Msg
  = Tick Int
  | Mouse P
  | Click MouseButton InputMotion P
  deriving stock (Show)

initialModel :: Model
initialModel =
  Model
    { planets = []
    }

subscriptions :: Model -> Sub Msg
subscriptions _ =
  mconcat
    [ Sub.micros Tick
    , Sub.mouseMotion Mouse
    , Sub.mouseClicks Click
    ]

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Click ButtonLeft Pressed point ->
      model { planets = Planet point 0 : planets model }

    _ ->
      model

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
