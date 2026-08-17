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
import Physics.Units.Constants
import Physics.Units.Convert
import Physics.Units.Planck ( One,
    Metre,
    Kilogram,
    Second,
    Coulomb,
    Kelvin,
    Siemens,
    Farad,
    Pascal,
    Hertz,
    Becquerel,
    Ampere,
    Radian,
    Steradian,
    Tesla,
    Newton,
    Gray,
    Sievert,
    Watt,
    Volt,
    Joule,
    Ohm,
    Weber,
    Henry,
  )
import Algebra.Ring 
import Algebra.Field
import qualified Algebra.Field as Field
import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.Absolute as Absolute
import MathObj.Wrapper.NumericPrelude
import Control.Applicative
import Data.Proxy
import GHC.TypeLits
import Data.Type.Equality
import NumericPrelude

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
