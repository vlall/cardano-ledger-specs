{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Byron.Spec.Chain.STS.Rule.Bupi where

import           Data.Data (Data, Typeable)


import           Control.State.Transition (Embed, Environment, PredicateFailure, STS, Signal, State,
                     TRC (TRC), TransitionRule, Event, initialRules, judgmentContext, trans,
                     transitionRules, wrapFailed, wrapEvents)
import           Byron.Spec.Ledger.Core (VKey)
import           Byron.Spec.Ledger.Update (ProtVer, UPIEND, UPIEnv, UPIREG, UPIState, UPIVOTES, UProp, Vote)


type UpdatePayload =
  ( Maybe UProp
  , [Vote]
  , (ProtVer, VKey)
  )


data BUPI deriving (Data, Typeable)

data BupiPredicateFailure
  = UPIREGFailure (PredicateFailure UPIREG)
  | UPIVOTESFailure (PredicateFailure UPIVOTES)
  | UPIENDFailure (PredicateFailure UPIEND)
  deriving (Eq, Show, Data, Typeable)

instance STS BUPI where
  type Environment BUPI = UPIEnv

  type State BUPI = UPIState

  type Signal BUPI = UpdatePayload

  type PredicateFailure BUPI = BupiPredicateFailure

  data Event _
    = UPIREGEvent  (Event UPIREG)
    | UPVOTESEvent (Event UPIVOTES)
    | UPIENDEvent  (Event UPIEND)

  initialRules = []

  transitionRules =
    [ do
        TRC (_, _, (mProp, _, _)) <- judgmentContext
        case mProp of
          Just prop -> hasProposalRule prop
          Nothing   -> noProposalRule
    ]
   where
    hasProposalRule :: UProp -> TransitionRule BUPI
    hasProposalRule prop = do
      TRC (env, us, (_, votes, end)) <- judgmentContext
      us'    <- trans @UPIREG   $ TRC (env, us  , prop)
      us''   <- trans @UPIVOTES $ TRC (env, us' , votes)
      us'''  <- trans @UPIEND   $ TRC (env, us'', end)
      return $! us'''

    noProposalRule :: TransitionRule BUPI
    noProposalRule = do
      TRC (env, us, (_, votes, end)) <- judgmentContext
      us'    <- trans @UPIVOTES $ TRC (env, us , votes)
      us''   <- trans @UPIEND   $ TRC (env, us', end)
      return $! us''


instance Embed UPIREG BUPI where
  wrapFailed = UPIREGFailure
  wrapEvents = UPIREGEvent

instance Embed UPIVOTES BUPI where
  wrapFailed = UPIVOTESFailure
  wrapEvents = UPVOTESEvent

instance Embed UPIEND BUPI where
  wrapFailed = UPIENDFailure
  wrapEvents = UPIENDEvent
