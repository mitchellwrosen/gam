module Gam.Internal.M where

import Gam.Internal.Prelude
import Internal.V              (V(..))

import Data.Vector.Unboxed (Vector, (!))

import qualified Data.Vector.Unboxed as Vector


data M
  = M (Vector Float)

instance Monoid M where
  mempty =
    M (Vector.fromList elems)
    where
      elems =
        [ 1, 0
        , 0, 1
        , 0, 0
        ]

  mappend =
    (<>)

instance Semigroup M where
  M m1 <> M m2 =
    M (Vector.fromList elems)
    where
      elems =
        [ a1*a2 + b1*d2,      a1*b2 + b1*e2
        , d1*a2 + e1*d2,      d1*b2 + e1*e2
        , g1*a2 + h1*d2 + g2, g1*b2 + h1*e2 + h2
        ]

      a1 = m1 ! 0
      b1 = m1 ! 1
      d1 = m1 ! 2
      e1 = m1 ! 3
      g1 = m1 ! 4
      h1 = m1 ! 5

      a2 = m2 ! 0
      b2 = m2 ! 1
      d2 = m2 ! 2
      e2 = m2 ! 3
      g2 = m2 ! 4
      h2 = m2 ! 5

rotate :: Float -> M
rotate r =
  M (Vector.fromList elems)
  where
    elems =
      [  c, s
      , -s, c
      ,  0, 0
      ]

    c = cos r
    s = sin r

rotate90 :: M
rotate90 =
  M (Vector.fromList elems)
  where
    elems =
      [  0, 1
      , -1, 0
      ,  0, 0
      ]

scale :: Float -> M
scale n =
  M (Vector.fromList elems)
  where
    elems =
      [ n, 0
      , 0, n
      , 0, 0
      ]

translate :: V -> M
translate (V x y) =
  M (Vector.fromList elems)
  where
    elems =
      [ 1, 0
      , 0, 1
      , x, y
      ]
