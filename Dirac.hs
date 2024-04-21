{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE DataKinds #-}

import Data.Complex

(<|) :: Ket → Label → TotalProbability
(|>) :: Label → Ket

type Bra = Ket → TotalProbability

(<| 0) (0 |>)

data ℝ⃰ -- Hyperreal

data ℂ⃰ = Complex ℝ⃰ -- Hypercomplex

feynman :: QuasiQuoter --(makes Probability, not TotalProbability since only one path)

data Basis = Position | Momentum

data Operator (b :: Basis)

x̂ :: Operator b
x̂ = 
