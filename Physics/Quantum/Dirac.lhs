> module Physics.Quantum.Dirac where

> import Data.Complex
> import Physics.Units.Planck

> type Ring n = Num n
> type Real n = Floating n
> type Differentiable n = RealFloat n

TODO: should make a type that contains coordinates of n dimensions along with the metric at that point

> newtype Wavefunction unit = Wavefunction { eval ∷ ∀r.Differentiable r ⇒ [Metre r] → unit (Complex r) }

Bra

Should have wavefunction track a rough bounding box

> (<|) ∷ Wavefunction unit → (∀r.Differentiable r ⇒ Wavefunction unit → unit r)
> (<|) φ ψ = undefined

Ket

> (|>) ∷ (∀r.Differentiable r ⇒ [Metre r] → unit (Complex r)) → Wavefunction unit
> (|>) = Wavefunction

> type Operator unit = Wavefunction One → Wavefunction unit
>
> position ∷ Operator Metre
> position = undefined
>
> momentum ∷ Operator (Kilogram >*< Metre >/< Second)
> momentum = undefined
