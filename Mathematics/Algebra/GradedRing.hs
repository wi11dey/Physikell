{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Mathematics.Algebra.GradedRing
  ( Grade,
    C,
    Unit,
    Product,
    Exponent,
    Power,
    PowerConstraint,
    (*),
    (^),
    one,
    fromInteger,
  )
where

import qualified Algebra.Additive as Additive
import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import qualified Numeric.NumType.DK.Integers as TypeInt
import qualified Numeric.Units.Dimensional as Dimensional
import qualified Numeric.Units.Dimensional.Coercion as Dimensional
import Prelude (Integer)

infixl 7 *
infixr 8 ^

class Grade ring => C (ring :: kind -> Type -> Type) coefficient where
  (*) :: ring left coefficient -> ring right coefficient -> ring (Product ring left right) coefficient

  (^)
    :: (Field.C coefficient, PowerConstraint ring exponent)
    => ring grade coefficient
    -> Proxy exponent
    -> ring (Power ring grade exponent) coefficient

  one :: ring (Unit ring) coefficient
  one = fromInteger 1

  fromInteger :: Integer -> ring (Unit ring) coefficient

  {-# MINIMAL (*), fromInteger #-}

class Grade (ring :: kind -> Type -> Type) where
  type Unit ring :: kind
  type Product ring (left :: kind) (right :: kind) :: kind
  type Exponent ring :: Type
  type Power ring (grade :: kind) (exponent :: Exponent ring) :: kind
  type PowerConstraint ring (exponent :: Exponent ring) :: Constraint

instance Grade Dimensional.Quantity where
  type Unit Dimensional.Quantity = Dimensional.DOne
  type Product Dimensional.Quantity left right = left Dimensional.* right
  type Exponent Dimensional.Quantity = TypeInt.TypeInt
  type Power Dimensional.Quantity grade exponent = grade Dimensional.^ exponent
  type PowerConstraint Dimensional.Quantity exponent = TypeInt.KnownTypeInt exponent

instance Ring.C coefficient => C Dimensional.Quantity coefficient where
  left * right =
    Dimensional.coerce
      (Dimensional.unQuantity left Ring.* Dimensional.unQuantity right)
  value ^ exponent =
    Dimensional.coerce
      (Dimensional.unQuantity value Field.^- TypeInt.toNum exponent)
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
