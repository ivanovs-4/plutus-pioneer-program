{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo),
                                       Validator,
                                       TxInfo (txInfoValidRange),
                                       mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Plutus.V1.Ledger.Interval (after, before)

import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), traceIfFalse, ($), (&&), (||))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx =
            (  traceIfFalse "beneficiary1's signature missing" signedByBen1
            && traceIfFalse "deadline reached" deadlineNotReached
            )
            ||
            (  traceIfFalse "beneficiary2's signature missing" signedByBen2
            && traceIfFalse "deadline not passed" deadlinePassed
            )

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBen1 :: Bool
    signedByBen1 = txSignedBy info $ beneficiary1 dat

    signedByBen2 :: Bool
    signedByBen2 = txSignedBy info $ beneficiary2 dat

    deadlineNotReached :: Bool
    deadlineNotReached = after (deadline dat) $ txInfoValidRange info

    deadlinePassed :: Bool
    deadlinePassed = before (deadline dat) $ txInfoValidRange info

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
