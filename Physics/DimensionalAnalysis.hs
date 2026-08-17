{-# LANGUAGE FlexibleContexts #-}

module Physics.DimensionalAnalysis
  ( (*<),
    (>+<),
    (>-<),
    (>*<),
    (>/<),
    square,
    unit,
    ħ,
    mₑ,
    module Physics.Units.Arithmetic,
    module Physics.Units.Type,
    module Physics.Units.Planck
  )
where

import Data.Coerce
import Data.Functor
import Physics.Units.Type
import Physics.Units.Arithmetic hiding ((*<), (>+<), (>-<), (>*<), (>/<), square, hypercube, unit)
import Physics.Units.Planck hiding ((*<), (>+<), (>-<), (>*<), (>/<), square, hypercube, unit)
import Physics.Units.Constants
import Physics.Units.Convert
import Algebra.Ring 
import Algebra.Field
import qualified Algebra.Field as Field
import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.DivisibleSpace as DivisibleSpace
import qualified Algebra.Absolute as Absolute
import MathObj.Wrapper.NumericPrelude
import Control.Applicative
import Data.Proxy
import GHC.TypeLits
import Data.Type.Equality
import NumericPrelude
import Algebra.Additive
import Algebra.Module
import qualified Algebra.Module as Module
import qualified Algebra.VectorSpace as VectorSpace
import Algebra.DivisibleSpace
import qualified Algebra.DivisibleSpace as DivisibleSpace

infixl 6 >+<, >-<
infixl 7 >*<, *<

unit ∷ (Functor f, f ~ Planck m kg s c k, Ring.C r) ⇒ f r
unit = Planck 1

(*<) ∷ (Ring.C x, Functor f) ⇒ x → f x → f x
x *< y = (x*) <$> y

(>*<) :: (Ring.C x, Coercible (f x) x, Coercible (f' x) x, Applicative (f>*<f')) => f x -> f' x -> (f >*< f') x
x >*< y = pure (coerce x * coerce y)

(>/<) :: (Field.C x, Coercible (f x) x, Coercible (f' x) x, Applicative (f>/<f')) => f x -> f' x -> (f >/< f') x
x >/< y = pure (coerce x / coerce y)

(>+<) ∷ (Additive.C x, Applicative f) ⇒ f x → f x → f x
x >+< y = (+) <$> x <*> y

(>-<) ∷ (Additive.C x, Applicative f) ⇒ f x → f x → f x
x >-< y = (-) <$> x <*> y

square :: (Coercible (f x) (Square f x), Ring.C x, Functor f) => f x -> Square f x
square = hypercube (Proxy :: Proxy 2)

ħ ∷ Ring.C n ⇒ (Joule >*< Second) n
ħ = undefined -- decons <$> fromSI reducedPlanckConstant

mₑ ∷ Transcendental.C n ⇒ Kilogram n
mₑ = undefined -- decons <$> fromSI electronMass

hypercube :: (KnownNat n, Ring.C x, Functor f, Coercible (f x) ((f^+n) x)) => Proxy n -> f x -> (f^+n) x
hypercube p = coerce . fmap (^natVal p)

instance Additive.C t ⇒ Additive.C (Planck m kg s c k t) where
  zero = Planck zero
  Planck x + Planck y = Planck (x + y)
  Planck x - Planck y = Planck (x - y)
  negate (Planck x) = Planck (negate x)

instance Ring.C t ⇒ Module.C t (Planck m kg s c k t) where
  x *> Planck y = Planck (x * y)

instance Field.C t ⇒ VectorSpace.C t (Planck m kg s c k t)

instance Field.C t ⇒ DivisibleSpace.C t (Planck m kg s c k t) where
  Planck x </> Planck y = x / y

