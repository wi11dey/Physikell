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
