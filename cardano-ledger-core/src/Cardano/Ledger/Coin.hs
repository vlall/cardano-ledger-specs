{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.Coin
  ( Coin (..),
    CompactForm (..),
    DeltaCoin (..),
    SubCoin (..),
    word64ToCoin,
    coinToRational,
    rationalToCoinViaFloor,
    addDeltaCoin,
    toDeltaCoin,
    integerToWord64,
    roundSubCoin,
    toSubCoin,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Compactible
import Cardano.Prelude (HeapWords)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Group (Abelian, Group (..))
import Data.Monoid (Sum (..))
import Data.PartialOrd (PartialOrd)
import Data.Proxy
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.TypeLits
import NoThunks.Class (NoThunks (..))
import Numeric.Decimal
import Quiet

-- | The amount of value held by a transaction output.
newtype Coin = Coin {unCoin :: Integer}
  deriving
    ( Eq,
      Ord,
      Enum,
      NoThunks,
      Generic,
      ToJSON,
      FromJSON,
      NFData
    )
  deriving (Show) via Quiet Coin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd, FromCBOR, ToCBOR, HeapWords)

newtype DeltaCoin = DeltaCoin Integer
  deriving (Eq, Ord, Generic, Enum, NoThunks, NFData, FromCBOR, ToCBOR, HeapWords)
  deriving (Show) via Quiet DeltaCoin
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (PartialOrd)

addDeltaCoin :: Coin -> DeltaCoin -> Coin
addDeltaCoin (Coin x) (DeltaCoin y) = Coin (x + y)

toDeltaCoin :: Coin -> DeltaCoin
toDeltaCoin (Coin x) = DeltaCoin x

word64ToCoin :: Word64 -> Coin
word64ToCoin w = Coin $ fromIntegral w

coinToRational :: Coin -> Rational
coinToRational (Coin c) = fromIntegral c

rationalToCoinViaFloor :: Rational -> Coin
rationalToCoinViaFloor r = Coin . floor $ r

instance Compactible Coin where
  newtype CompactForm Coin = CompactCoin Word64
    deriving (Eq, Show, NoThunks, NFData, Typeable, HeapWords)

  toCompact (Coin c) = CompactCoin <$> integerToWord64 c
  fromCompact (CompactCoin c) = word64ToCoin c

-- It's odd for this to live here. Where should it go?
integerToWord64 :: Integer -> Maybe Word64
integerToWord64 = fromIntegerBounded

instance ToCBOR (CompactForm Coin) where
  toCBOR (CompactCoin c) = toCBOR c

instance FromCBOR (CompactForm Coin) where
  fromCBOR = CompactCoin <$> fromCBOR

type SubCoinRounding = Ceiling

-- | A `Coin` that has 9 digits of precision after the decimal point.
newtype SubCoin = SubCoin (Decimal SubCoinRounding 9 Integer)
  deriving (Eq, Generic)
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving newtype (Show, Ord, FromCBOR, ToCBOR, NoThunks, NFData)

-- TODO: move these instances into cardano-binary
deriving instance (Typeable r, KnownNat s) => ToCBOR (Decimal r s Integer)

deriving instance (Typeable r, KnownNat s) => FromCBOR (Decimal r s Integer)

deriving instance NoThunks p => NoThunks (Decimal r s p)

roundSubCoin :: SubCoin -> Coin
roundSubCoin (SubCoin d) = Coin (unwrapDecimal (roundDecimal d :: Decimal SubCoinRounding 0 Integer))

toSubCoin :: Coin -> SubCoin
toSubCoin (Coin c) = SubCoin (fromIntegerDecimal c)

------------------------------------------------------------------------------------------
-- `Ceiling` type and its `Round` instance have been added to safe-decimal-0.2.2 and can
-- be removed once upgraded
-----------

data Ceiling

instance Round Ceiling Integer where
  roundDecimal = roundUp
  {-# INLINEABLE roundDecimal #-}

roundUp :: forall r n k p. (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundUp (Decimal x)
  | x >= 0 && r /= 0 = Decimal (q + 1)
  | otherwise = Decimal q
  where
    k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
    (q, r) = quotRem x (10 ^ k)
{-# INLINEABLE roundUp #-}

------------------------------------------------------------------------------------------
