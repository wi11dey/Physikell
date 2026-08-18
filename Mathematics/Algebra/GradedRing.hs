{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Rings whose multiplication combines, rather than preserves, grades.
--
-- The grade kind is intentionally polymorphic.  It may be a natural number,
-- an integer exponent, a promoted dimension record, or any other kind chosen
-- by an instance.  Addition remains internal to each homogeneous component.
module Mathematics.Algebra.GradedRing
  ( Grading,
    C,
    Unit,
    Product,
    (*),
    one,
    fromInteger,
  )
where

import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import Data.Kind (Type)
import qualified Numeric.Units.Dimensional as Dimensional
import qualified Numeric.Units.Dimensional.Coercion as Dimensional
import Prelude (Integer)

infixl 7 *

-- | A grade-indexed ring.
--
-- Laws, in addition to the 'Additive.C' laws for each component:
--
-- * multiplication is associative (up to the reductions of 'Product');
-- * 'one' is a left and right identity (up to 'Unit' and 'Product');
-- * multiplication distributes over addition;
-- * 'fromInteger' embeds integers in the component of grade 'Unit'.
--
-- The coefficient is a separate parameter so that families such as
-- @Quantity :: Dimension -> Type -> Type@ can be instances directly.
class Grading ring => C (ring :: kind -> Type -> Type) coefficient where
  -- | Multiply homogeneous values, composing their grades.
  (*)
    :: ring left coefficient
    -> ring right coefficient
    -> ring (Product ring left right) coefficient

  -- | The multiplicative identity, which lies in the neutral component.
  one :: ring (Unit ring) coefficient
  one = fromInteger 1

  -- | Embed an integer in the neutral component.
  fromInteger :: Integer -> ring (Unit ring) coefficient

  {-# MINIMAL (*), fromInteger #-}

-- | The multiplicative structure of a grade kind.
--
-- Keeping this independent of 'C' allows one graded family to be used with
-- many coefficient types without repeating associated type instances.
class Grading (ring :: kind -> Type -> Type) where
  -- | The neutral grade for multiplication.
  type Unit ring :: kind

  -- | Composition of two grades under multiplication.
  type Product ring (left :: kind) (right :: kind) :: kind

instance Grading Dimensional.Quantity where
  type Unit Dimensional.Quantity = Dimensional.DOne
  type Product Dimensional.Quantity left right = left Dimensional.* right

instance Ring.C coefficient => C Dimensional.Quantity coefficient where
  left * right =
    Dimensional.coerce
      (Dimensional.unQuantity left Ring.* Dimensional.unQuantity right)
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
