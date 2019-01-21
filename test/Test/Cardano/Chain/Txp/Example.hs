{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Cardano.Chain.Txp.Example
  ( exampleTxId
  , exampleTxInList
  , exampleTxInUnknown
  , exampleTxInUtxo
  , exampleTxPayload
  , exampleTxProof
  , exampleTxOut
  , exampleTxOutList
  , exampleTxSig
  , exampleTxSigData
  , exampleTxpUndo
  , exampleTxWitness
  , exampleRedeemSignature
  , exampleHashTx
  )
where

import Cardano.Prelude

import Data.Coerce (coerce)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromJust)
import qualified Data.Vector as V

import Cardano.Chain.Common
  ( IsBootstrapEraAddr(..)
  , makePubKeyAddress
  , mkAttributes
  , mkKnownLovelace
  , mkMerkleTree
  , mtRoot
  )
import Cardano.Chain.Txp
  ( Tx(..)
  , TxAux
  , TxId
  , TxIn(..)
  , TxInWitness(..)
  , TxOut(..)
  , TxOutAux(..)
  , TxPayload
  , TxProof(..)
  , TxSig
  , TxSigData(..)
  , TxWitness
  , TxpUndo
  , mkTxAux
  , mkTxPayload
  )
import Cardano.Crypto
  ( AbstractHash(..)
  , Hash
  , ProtocolMagicId(..)
  , PublicKey(..)
  , RedeemSignature
  , SignTag(..)
  , hash
  , redeemDeterministicKeyGen
  , redeemSign
  , sign
  )
import qualified Cardano.Crypto.Wallet as CC

import Test.Cardano.Crypto.Bi (getBytes)
import Test.Cardano.Crypto.Example (examplePublicKey, exampleSecretKey)


exampleTxAux :: TxAux
exampleTxAux = mkTxAux tx exampleTxWitness
  where tx = UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ())

exampleTxId :: TxId
exampleTxId = exampleHashTx

exampleTxInList :: (NonEmpty TxIn)
exampleTxInList = fromList [exampleTxInUtxo]

exampleTxInUnknown :: TxIn
exampleTxInUnknown = TxInUnknown 47 ("forty seven" :: ByteString)

exampleTxInUtxo :: TxIn
exampleTxInUtxo = TxInUtxo exampleHashTx 47 -- TODO: loop here

exampleTxOut :: TxOut
exampleTxOut = TxOut
  (makePubKeyAddress (IsBootstrapEraAddr True) pkey)
  (mkKnownLovelace @47)
  where Right pkey = PublicKey <$> CC.xpub (getBytes 0 64)

exampleTxOutList :: (NonEmpty TxOut)
exampleTxOutList = fromList [exampleTxOut]

exampleTxPayload :: TxPayload
exampleTxPayload = mkTxPayload [exampleTxAux]

exampleTxProof :: TxProof
exampleTxProof = TxProof 32 mroot hashWit
 where
  mroot = mtRoot $ mkMerkleTree
    [(UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ()))]
  hashWit = hash $ [(V.fromList [(PkWitness examplePublicKey exampleTxSig)])]

exampleTxSig :: TxSig
exampleTxSig = sign
  (ProtocolMagicId 0)
  SignForTestingOnly
  exampleSecretKey
  exampleTxSigData

exampleTxSigData :: TxSigData
exampleTxSigData = TxSigData exampleHashTx

exampleTxpUndo :: TxpUndo
exampleTxpUndo = [Just . TxOutAux <$> exampleTxOutList]

exampleTxWitness :: TxWitness
exampleTxWitness = V.fromList [(PkWitness examplePublicKey exampleTxSig)]

exampleRedeemSignature :: RedeemSignature TxSigData
exampleRedeemSignature = redeemSign
  (ProtocolMagicId 0)
  SignForTestingOnly
  rsk
  exampleTxSigData
  where rsk = fromJust (snd <$> redeemDeterministicKeyGen (getBytes 0 32))

exampleHashTx :: Hash Tx
exampleHashTx = coerce (hash "golden" :: Hash Text)
