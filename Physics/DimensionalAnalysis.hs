module Physics.DimensionalAnalysis
  ( One,
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
    one,
    metre,
    kilogram,
    second,
    coulomb,
    kelvin,
    siemens,
    farad,
    pascal,
    hertz,
    becquerel,
    ampere,
    radian,
    steradian,
    tesla,
    newton,
    gray,
    sievert,
    watt,
    volt,
    joule,
    ohm,
    weber,
    henry,
    (*<),
    ħ,
    mₑ,
    module Physics.Units.Arithmetic,
    module Physics.Units.Type
  )
where

import Data.Functor
import Physics.Units.Type
import Physics.Units.Arithmetic hiding ((*<))
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
import Algebra.Ring hiding (one)
import qualified Algebra.Ring as Ring
import qualified Algebra.Transcendental as Transcendental
import qualified Algebra.Absolute as Absolute
import MathObj.Wrapper.NumericPrelude
import Prelude (Eq, Show)

infixl 7 *<

one       ∷ Ring.C a ⇒ One       a; one       = Planck 1
metre     ∷ Ring.C a ⇒ Metre     a; metre     = Planck 1
kilogram  ∷ Ring.C a ⇒ Kilogram  a; kilogram  = Planck 1
second    ∷ Ring.C a ⇒ Second    a; second    = Planck 1
coulomb   ∷ Ring.C a ⇒ Coulomb   a; coulomb   = Planck 1
kelvin    ∷ Ring.C a ⇒ Kelvin    a; kelvin    = Planck 1
siemens   ∷ Ring.C a ⇒ Siemens   a; siemens   = Planck 1
farad     ∷ Ring.C a ⇒ Farad     a; farad     = Planck 1
pascal    ∷ Ring.C a ⇒ Pascal    a; pascal    = Planck 1
hertz     ∷ Ring.C a ⇒ Hertz     a; hertz     = Planck 1
becquerel ∷ Ring.C a ⇒ Becquerel a; becquerel = Planck 1
ampere    ∷ Ring.C a ⇒ Ampere    a; ampere    = Planck 1
radian    ∷ Ring.C a ⇒ Radian    a; radian    = Planck 1
steradian ∷ Ring.C a ⇒ Steradian a; steradian = Planck 1
tesla     ∷ Ring.C a ⇒ Tesla     a; tesla     = Planck 1
newton    ∷ Ring.C a ⇒ Newton    a; newton    = Planck 1
gray      ∷ Ring.C a ⇒ Gray      a; gray      = Planck 1
sievert   ∷ Ring.C a ⇒ Sievert   a; sievert   = Planck 1
watt      ∷ Ring.C a ⇒ Watt      a; watt      = Planck 1
volt      ∷ Ring.C a ⇒ Volt      a; volt      = Planck 1
joule     ∷ Ring.C a ⇒ Joule     a; joule     = Planck 1
ohm       ∷ Ring.C a ⇒ Ohm       a; ohm       = Planck 1
weber     ∷ Ring.C a ⇒ Weber     a; weber     = Planck 1
henry     ∷ Ring.C a ⇒ Henry     a; henry     = Planck 1

(*<) ∷ (Ring.C x, Functor f) ⇒ x → f x → f x
x *< y = (x*) <$> y

ħ ∷ (Transcendental.C n, Absolute.C n, Eq n, Show n) ⇒ (Joule >*< Second) n
ħ = decons <$> fromSI reducedPlanckConstant

mₑ ∷ (Transcendental.C n, Absolute.C n, Eq n, Show n) ⇒ Kilogram n
mₑ = decons <$> fromSI electronMass
