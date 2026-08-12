> module Schrodinger where

> import Physics.Units.Planck
> import Physics.Units.Constants
> import Physics.Units.Convert
> import Data.Complex
> import Data.Functor
> import Data.Functor.Identity
> import Numeric.AD

> type Differentiable n = RealFloat n

TODO: should make a type that contains coordinates of n dimensions along with the metric at that point

> type Wavefunction u = ∀r.Differentiable r ⇒ [Metre r] → u (Complex r)

> dimensionless ∷ Wavefunction Identity → (∀r. Differentiable r ⇒ [Metre r] → Complex r)
> dimensionless = (runIdentity .)

> ħ ∷ Floating a ⇒ (Joule >*< Second) a
> ħ = fromSI reducedPlanckConstant

> m_e ∷ Floating a ⇒ Kilogram a
> m_e = fromSI electronMass

> tr ∷ Num t ⇒ [[t]] → t
> tr matrix = sum (zipWith (!!) matrix [0..])

> volume ∷ Num r ⇒ [r] → [Metre r]
> volume = fmap (*< metre)

> laplacian ∷ (Functor g, Differentiable a, Fractional (g a)) ⇒ (∀r.Differentiable r ⇒ [Metre r] → g r) → [Metre a] → (One >/< Square Metre) (g a)
> laplacian f (fmap value → x) = (tr <$> hessianF (f . volume) x)/<square metre

H|psi> = (T + V)|psi>

iħ d/dt |psi x t> = H |psi>

(hat H) |psi>

|psi> :: R^3 -> C

> hamiltonian ∷ (∀r.Differentiable r ⇒ [Metre r] → Joule r) → (∀r.Differentiable r ⇒ Kilogram r) → Wavefunction Identity → Wavefunction Joule
> hamiltonian potential (fmap pure → m) (dimensionless → ψ) x =
>   square ħ>/<(pure (-2)*<m) >*< laplacian ψ x >+< (ψ x *< pure <$> potential x)
