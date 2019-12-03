{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Elaboration.Delegation
  ( elaborateDCert
  , elaborateDSEnv
  , tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.Set as Set
import Hedgehog (assert, forAll, property, success, cover)

import Cardano.Chain.Common (hashKey)
import qualified Cardano.Chain.Common as Concrete
import qualified Cardano.Chain.Delegation as Concrete
import qualified Cardano.Chain.Delegation as Concrete.Certificate
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as Concrete
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Crypto.Signing (noPassSafeSigner)
import Ledger.Core
  ( BlockCount(..)
  , Epoch(..)
  , Owner(..)
  , Slot(..)
  , VKey(..)
  , VKeyGenesis(..)
  )
import Ledger.Delegation (DCert(..), DSEnv(..), dcertGen, delegate, delegator)

import Test.Cardano.Chain.Config (readMainetCfg)
import Test.Cardano.Chain.Elaboration.Keys
  (elaborateKeyPair, elaborateVKeyGenesis, vKeyPair)
import qualified Test.Cardano.Crypto.Dummy as Dummy
import Test.Options (TSGroup, TSProperty, withTestsTS)


tests :: TSGroup
tests = $$discoverPropArg


ts_prop_elaboratedCertsValid :: TSProperty
ts_prop_elaboratedCertsValid =
  withTestsTS 50
    . property
    $ do
        config <- readMainetCfg

        let pm = Genesis.configProtocolMagicId config

        -- Generate and elaborate a certificate
        mCert <- forAll $ dcertGen env Set.empty

        cover 95
          "A certificate was generated"
          (isJust mCert)

        -- Validate the certificate
        case mCert of
          Nothing ->
            success -- We ignore 'Nothing' values when we the signal generator
                    -- fails. Coverage testing ensures we will not generate a
                    -- large portion of 'Nothing'.
          Just cert ->
            let concreteCert = elaborateDCert pm cert in
            assert
              $ Concrete.Certificate.isValid pm concreteCert
 where
  env = DSEnv
    { _dSEnvAllowedDelegators = Set.fromList
      . fmap (VKeyGenesis . VKey . Owner)
      $ [0 .. 6]
    , _dSEnvEpoch       = Epoch 0
    , _dSEnvSlot        = Slot 0
    , _dSEnvK           = BlockCount 2160
    }


elaborateDCert :: ProtocolMagicId -> DCert -> Concrete.Certificate
elaborateDCert pm cert = Concrete.signCertificate
  pm
  delegateVK
  epochNo
  (noPassSafeSigner delegatorSK)
 where
  VKeyGenesis delegatorVKey = delegator cert
  (_         , delegatorSK) = elaborateKeyPair $ vKeyPair delegatorVKey
  (delegateVK, _          ) = elaborateKeyPair . vKeyPair $ delegate cert

  Epoch e = depoch cert

  epochNo :: Concrete.EpochNumber
  epochNo = fromIntegral e

elaborateDSEnv :: DSEnv -> Scheduling.Environment
elaborateDSEnv abstractEnv = Scheduling.Environment
  { Scheduling.protocolMagic = Dummy.protocolMagicId
  , Scheduling.allowedDelegators = Set.fromList
    $   hashKey
    .   elaborateVKeyGenesis
    <$> Set.toList genesisKeys
  , Scheduling.currentEpoch = fromIntegral e
  , Scheduling.currentSlot = Concrete.SlotNumber s
  , Scheduling.k           = Concrete.BlockCount k
  }
  where
    DSEnv genesisKeys (Epoch e) (Slot s) (BlockCount k) = abstractEnv
