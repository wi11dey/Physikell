I try to stay as close to how physicists write as possible within the bounds of Haskell, and then try to maintain as much numerical accuracy while staying within those bounds

> module Physics.Quantum.Mechanics where

> import Physics.Quantum.Dirac
> import Physics.DimensionalAnalysis
> import Data.Traversable
> import Data.Complex
> import Data.Functor
> import Data.Coerce
> import Numeric.AD hiding (hessian)
> import qualified Numeric.AD
> import NumericPrelude hiding ((*>))
> import Mathematics.Constants (π)
> import qualified Algebra.Transcendental as Transcendental
> import qualified Algebra.RealTranscendental as RealTranscendental
> import qualified Algebra.Ring as Ring
> import qualified Number.Complex as ℂ
> import qualified Algebra.Absolute as Absolute
> import Control.Applicative (pure)

> i ∷ Ring.C r ⇒ ℂ.T r
> i = ℂ.imaginaryUnit

> dimensionless ∷ Wavefunction One → (∀r.RealTranscendental.C r ⇒ [Metre r] → ℂ.T r)
> dimensionless = (value .) . eval

> gaussian ∷ (∀r.RealTranscendental.C r ⇒ r) → (∀r.RealTranscendental.C r ⇒ r) → (∀r.(RealTranscendental.C r, Coercible (unit r) r) ⇒ [unit r] → One (ℂ.T r))
> gaussian μ σ (fmap value → x) = pure $ ℂ.fromReal $
>   1/(sqrt 2*π*σ^2)**(fromIntegral (length x)/4) * (exp $ -(sum $ x <&> (-) μ <&> (^2))/(2*σ^2))

> tr ∷ Ring.C t ⇒ [[t]] → t
> tr matrix = sum (zipWith (!!) matrix [0..])

> volume ∷ Ring.C r ⇒ [r] → [Metre r]
> volume = fmap (*> unit @Metre)

> hessian :: (Traversable f, Functor g) ⇒ (∀r. RealTranscendental.C r ⇒ f r → g r) → f r → g (f (f r))
> hessian f = undefined

> laplacian ∷ (Functor g, RealTranscendental.C a) ⇒ (∀r.RealTranscendental.C r ⇒ [Metre r] → g r) → [Metre a] → (One >/< Square Metre) (g a)
> laplacian f (fmap value → x) = Planck (tr <$> hessian (f . volume) x)

H|psi> = (T + V)|psi>

iħ d/dt |psi x t> = H |psi>

(hat H) |psi>

|psi> :: R^3 -> C

> singleParticleH ∷ (∀r.RealTranscendental.C r ⇒ [Metre r] → Joule r) → (∀r.RealTranscendental.C r ⇒ Kilogram r) → Wavefunction One → Wavefunction Joule
> singleParticleH potential m (dimensionless → ψ) = ((\x →
>   square ħ>/<(ℂ.fromReal <$> (-2)*>m) >*< laplacian ψ x + (ψ x *> (ℂ.fromReal <$> potential x))) |>)

= Integration

> integrate ∷ (∀r.RealTranscendental.C r ⇒ Second r) → (Wavefunction One → Wavefunction Joule) → Wavefunction One → [Wavefunction One]
> integrate stepSize hamiltonian ψ = (ψ:) $ integrate stepSize hamiltonian $ ((\x → pure $
>   dimensionless ψ x - i*(value $ hamiltonian ψ `eval` x >/< ħ >*< (ℂ.fromReal <$> stepSize))) |>)

> test = integrate (0.00001*>unit @Second) (singleParticleH (const $ 0*>unit @Joule) mₑ) (gaussian 0 0.00005 |>)

Consider bold(Crank–Nicolson), split-operator evolution, or a matrix exponential.

All three improve on forward Euler by respecting the oscillatory, unitary nature of Schrödinger evolution.

=== Crank–Nicolson

It averages the Hamiltonian derivative at the beginning and end of each timestep:

\[
\left(I+\frac{i\Delta t}{2\hbar}H\right)\psi_{n+1}
=
\left(I-\frac{i\Delta t}{2\hbar}H\right)\psi_n
\]

So each step requires solving a linear system:

\[
\psi_{n+1}
=
\left(I+\frac{i\Delta t}{2\hbar}H\right)^{-1}
\left(I-\frac{i\Delta t}{2\hbar}H\right)\psi_n
\]

Advantages:

- Second-order accurate in time.
- Stable for any timestep.
- Exactly norm-preserving for a Hermitian discretized Hamiltonian.
- Works well with finite-difference spatial grids.

Its main cost is solving a potentially large sparse linear system every step. For a 1D finite-difference Hamiltonian, that matrix is tridiagonal and inexpensive to solve.

=== Split-operator evolution

When the Hamiltonian separates as

\[
H=T+V
\]

the short-time evolution can be approximated by Strang splitting:

\[
\psi(t+\Delta t)
\approx
e^{-iV\Delta t/(2\hbar)}
e^{-iT\Delta t/\hbar}
e^{-iV\Delta t/(2\hbar)}
\psi(t)
\]

The potential operator is diagonal in position space:

\[
\psi(x)\mapsto
e^{-iV(x)\Delta t/(2\hbar)}\psi(x)
\]

The kinetic operator is diagonal in momentum space:

\[
\tilde\psi(p)\mapsto
e^{-ip^2\Delta t/(2m\hbar)}\tilde\psi(p)
\]

Consequently, one step is:

1. Multiply by half the potential phase in position space.
2. Fourier-transform to momentum space.
3. Multiply by the kinetic phase.
4. Inverse Fourier-transform.
5. Multiply by the remaining potential half-phase.

Advantages:

- Second-order accurate.
- Norm-preserving.
- Very fast using FFTs: \(O(N\log N)\) per step.
- Particularly good for regular periodic grids and \(T+V(x)\) Hamiltonians.

It becomes less convenient with irregular geometry, position-dependent mass, nonlocal potentials, or Hamiltonians that do not separate cleanly.

=== Matrix exponential

For a time-independent discretized Hamiltonian, the exact timestep is

\[
\psi_{n+1}
=
e^{-iH\Delta t/\hbar}\psi_n
\]

This is the most direct expression of quantum evolution. If \(H\) is Hermitian, the exponential is unitary and preserves both norm and energy.

For small matrices, you can compute the exponential itself. If

\[
H=U D U^\dagger
\]

then

\[
e^{-iH\Delta t/\hbar}
=
Ue^{-iD\Delta t/\hbar}U^\dagger
\]

For large systems, forming the dense exponential is prohibitively expensive. Instead, Krylov methods approximate only its action on the state:

\[
e^{-iH\Delta t/\hbar}\psi
\]

without constructing the full exponential.

Advantages:

- Exact in time apart from numerical approximation.
- Norm-preserving when computed accurately.
- Excellent for time-independent Hamiltonians.
- Krylov methods work well with large sparse matrices.

The main limitation is that your current `Wavefunction` is represented as a polymorphic function. These algorithms normally require first discretizing space so that a wavefunction becomes a vector and the Hamiltonian becomes a matrix or linear operator.

For your project, I’d start with Crank–Nicolson on a one-dimensional finite grid. It exposes the spatial discretization clearly and does not require implementing FFTs immediately.
