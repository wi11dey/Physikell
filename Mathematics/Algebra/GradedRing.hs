{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Mathematics.Algebra.GradedRing
  ( Grade,
    C,
    Unit,
    Product,
    Power,
    (*),
    (^),
    one,
    fromInteger,
  )
where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import Data.Kind (Type)
import Data.Proxy (Proxy)
import GHC.TypeLits (KnownNat, Nat, natVal)
import qualified GHC.TypeNats as Nat
import qualified Numeric.Units.Dimensional as Dimensional
import qualified Numeric.Units.Dimensional.Coercion as Dimensional
import Prelude (Integer)
import qualified Prelude as P

infixl 7 *
infixr 8 ^

class Grade ring => C (ring :: kind -> Type -> Type) coefficient where
  (*) :: ring left coefficient -> ring right coefficient -> ring (Product ring left right) coefficient

  (^)
    :: KnownNat exponent
    => ring grade coefficient
    -> Proxy exponent
    -> ring (Power ring grade exponent) coefficient

  one :: ring (Unit ring) coefficient
  one = fromInteger 1

  fromInteger :: Integer -> ring (Unit ring) coefficient

  {-# MINIMAL (*), (^), fromInteger #-}

class Grade (ring :: kind -> Type -> Type) where
  type Unit ring :: kind
  type Product ring (left :: kind) (right :: kind) :: kind
  type Power ring (grade :: kind) (exponent :: Nat) :: kind

type family QuantityPower (grade :: Dimensional.Dimension) (exponent :: Nat) :: Dimensional.Dimension where
  QuantityPower grade 0 = Dimensional.DOne
  QuantityPower grade exponent =
    grade Dimensional.* QuantityPower grade (exponent Nat.- 1)

instance Grade Dimensional.Quantity where
  type Unit Dimensional.Quantity = Dimensional.DOne
  type Product Dimensional.Quantity left right = left Dimensional.* right
  type Power Dimensional.Quantity grade exponent =
    QuantityPower grade exponent

instance Ring.C coefficient => C Dimensional.Quantity coefficient where
  left * right =
    Dimensional.coerce
      (Dimensional.unQuantity left Ring.* Dimensional.unQuantity right)
  value ^ exponent =
    Dimensional.coerce
      (Dimensional.unQuantity value Ring.^ P.toInteger (natVal exponent))
  fromInteger number =
    Dimensional.coerce (Ring.fromInteger number :: coefficient)

instance Additive.C coefficient => Additive.C (Dimensional.Quantity grade coefficient) where
  zero = Dimensional.coerce (Additive.zero :: coefficient)
  left + right =
    Dimensional.coerce
      (Dimensional.unQuantity left Additive.+ Dimensional.unQuantity right)
  left - right =
    Dimensional.coerce
      (Dimensional.unQuantity left Additive.- Dimensional.unQuantity right)
  negate value =
    Dimensional.coerce (Additive.negate (Dimensional.unQuantity value))
