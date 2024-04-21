{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TupleSections #-}

class Script s
  fromSubscript :: Subscript → s
  fromSuperscript :: Superscript → s

instance Script Subscript where
  fromSubscript = id
  fromSuperscript = g ^ i ^ j * 

instance Script Superscript

class Curvature t where
  curvature :: t

instance Curvature ℝ where
  curvature =
    [g ^ i ^ j * R __ i __ j |
      i ← indices [0..],
      j ← indices [0..]]

instance Curvature (Index → Index → ℝ) where
  curvature i j =
    [R ^ k __ i __ k __ j |
      k ← indices [1..]]

instance Curvature (Index → Index → Index → Index → ℝ) where
  curvature = riemannTensor

pattern R ← RiemannCurvature where
  R = curvature

pattern T ← () where
  T = massEnergyTensor

newtype Component = Component Int
  deriving Enum

instance Bounded Component where
  minBound = 0
  maxBound = 3

indices :: [Component] → [Script]
indices = flip map [0..] . flip Script

data Script = Script Int [Component]

data Variance = Covariant | Contravariant

data Index = Index Script Variance

newtype Variable = Variable String

data Expression =
  Constant ℂ |
  Sum [Component] Variable Expression |
  Variable Variable |
  Expression :+: Expression |
  Expression :*: Expression |
  Negate Expression |
  Reciprocal Expression

instance IsLabel v Expression where
  fromLabel = Variable $ symbolVal (Proxy :: Proxy v)

data Covector t = Covector Index t

instance Tensor (Covector t) where
  superscript = id
  subscript tensor i = #g __ i __ j * tensor ^ j

instance Tensor (Vector t) where
  superscript tensor i = #g ^ i ^ j * tensor __ j
  subscript = id

pattern t __ i ← Covector i t where
  t __ i = subscript t i

pattern t ^ i ← Vector i t where
  t ^ i = superscript t i

Covector j (Covector k (Vector i Γ))

define Γ ^ i __ k __ l = 1/2 * #g ^ i ^ m * (#g __ m __ k __ (, l) + #g __ m __ l __ (, k) - #g __ k __ l __ (, m))

 #g __ k __ l ^ (；m)

efe =
  [R __ μ __ ν - 1/2 * R * g __ μ __ ν + #Λ * g __ μ __ ν ≡ 8 * π * #G / c^4 * #T __ μ __ ν |
    μ ← index [0..],
    ν ← index [0..]]

(__) :: (Index → a) → Script → a
x __ i = x (Index i Covaraint)

class Superscriptable base superscript result where
  (^) :: base → superscript → result

instance Superscriptable (Index → a) Script a where
  x ^ i = x (Index i Contravariant)

instance Num a ⇒ Superscriptable a a a

scalarField = #T __ μ __ ν ≡ #ϕ __ (, μ) * #ϕ __ (, ν) + 1/2 * η __ μ __ ν * #ϕ __ (, σ) * #ϕ ^ (, σ) + m/2 * #ϕ ^ 2 * η __ μ __ ν

type Derivative = a → (a, Index)

type CovariantDerivative = CovariantDerivativePrefix → Index
data CovariantDerivativePrefix
(；) :: CovariantDerivativePrefix → Index → Index
(；) = const id
