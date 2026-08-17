> module Physics.Quantum.Dirac where

> import Physics.Units.Planck
> import NumericPrelude
> import qualified Algebra.RealTranscendental as RealTranscendental
> import qualified Number.Complex as ℂ

TODO: should make a type that contains coordinates of n dimensions along with the metric at that point

> newtype Wavefunction unit = Wavefunction { eval ∷ ∀r.RealTranscendental.C r ⇒ [Metre r] → unit (ℂ.T r) }

Bra

Should have wavefunction track a rough bounding box

> (<|) ∷ Wavefunction unit → (∀r.RealTranscendental.C r ⇒ Wavefunction unit → unit r)
> (<|) φ ψ = undefined

Ket

> (|>) ∷ (∀r.RealTranscendental.C r ⇒ [Metre r] → unit (ℂ.T r)) → Wavefunction unit
> (|>) = Wavefunction

> type Operator unit = Wavefunction One → Wavefunction unit
>
> position ∷ Operator Metre
> position = undefined
>
> momentum ∷ Operator (Kilogram >*< Metre >/< Second)
> momentum = undefined
