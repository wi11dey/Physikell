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

> real ∷ Num r ⇒ r → Complex r
> real = (:+ 0)

> π ∷ Floating a ⇒ a
> π = pi

> ħ ∷ Floating a ⇒ (Joule >*< Second) a
> ħ = fromSI reducedPlanckConstant

> m_e ∷ Floating a ⇒ Kilogram a
> m_e = fromSI electronMass

> dimensionless ∷ Wavefunction Identity → (∀r.Differentiable r ⇒ [Metre r] → Complex r)
> dimensionless = (runIdentity .)

> gaussian ∷ (∀r.Differentiable r ⇒ r) → (∀r.Differentiable r ⇒ r) → Wavefunction Identity
> gaussian μ σ (fmap value → x) = pure $ real $
>   1/(sqrt $ 2*π*σ^2)**(fromIntegral (length x)/4) * (exp $ -(sum $ x <&> (-) μ <&> (^2))/(2*σ^2))

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

> singleParticleH ∷ (∀r.Differentiable r ⇒ [Metre r] → Joule r) → (∀r.Differentiable r ⇒ Kilogram r) → Wavefunction Identity → Wavefunction Joule
> singleParticleH potential (fmap real → m) (dimensionless → ψ) x =
>   square ħ>/<(real (-2)*<m) >*< laplacian ψ x >+< (ψ x *< real <$> potential x)

= Integration

> integrate ∷ (∀r.Differentiable r ⇒ Second r) → (Wavefunction Identity → Wavefunction Joule) → Wavefunction Identity → [Wavefunction Identity]
> integrate stepSize hamiltonian ψ₀ = ψ₀ : integrate stepSize hamiltonian ψ₀ -- TODO

> test = integrate (1*<second) (singleParticleH (const $ 0 *< joule) m_e) (gaussian 1 0.5)
