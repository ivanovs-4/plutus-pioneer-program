{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Homework
    ( stakeValidator'
    , saveStakeValidator'
    , pkhFromString
    ) where

import qualified Data.ByteString.Char8 as BS8
import           Plutus.V1.Ledger.Value (valueOf)
import           Plutus.V2.Ledger.Api (Address, BuiltinData, PubKeyHash (PubKeyHash),
                                       StakingCredential,
                                       ScriptContext (scriptContextPurpose, scriptContextTxInfo),
                                       ScriptPurpose (Certifying, Rewarding),
                                       StakeValidator,
                                       TxInfo (txInfoOutputs, txInfoWdrl),
                                       TxOut (txOutAddress, txOutValue),
                                       adaSymbol, adaToken,
                                       toBuiltin,
                                       mkStakeValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import qualified PlutusTx
import qualified PlutusTx.AssocMap     as PlutusTx
import           PlutusTx.Prelude     (Bool (..), ($), (&&),
                                       Semigroup ((<>)),
                                       AdditiveSemigroup ((+)), Eq ((==)),
                                       MultiplicativeSemigroup ((*)),
                                       Integer, Maybe (Just, Nothing),
                                       Ord ((>=)), foldl,
                                       otherwise, traceError, traceIfFalse
                                       )
import           Prelude              (IO, String, Applicative(..), ioError, (.))
import           System.IO.Error      (userError)
import           Utilities            (bytesFromHex, tryReadAddress,
                                       wrapStakeValidator, writeStakeValidatorToFile)

-- | A staking validator with two parameters, a pubkey hash and an address. The validator
--   should work as follows:
--   1.) The given pubkey hash needs to sign all transactions involving this validator.
--   2.) The given address needs to receive at least half of all withdrawn rewards.
{-# INLINABLE mkStakeValidator' #-}
mkStakeValidator' :: PubKeyHash -> Address -> () -> ScriptContext -> Bool
mkStakeValidator' pkh addr () ctx = case scriptContextPurpose ctx of
    Certifying _   -> True
    Rewarding cred -> (traceIfFalse "missed pubkey signature" $
                          txSignedBy (scriptContextTxInfo ctx) pkh)
                   && (traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred)
    _              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    amount :: StakingCredential -> Integer
    amount cred = case PlutusTx.lookup cred $ txInfoWdrl info of
        Just amt -> amt
        Nothing  -> traceError "withdrawal not found"

    paidToAddress :: Integer
    paidToAddress = foldl f 0 $ txInfoOutputs info
      where
        f :: Integer -> TxOut -> Integer
        f n o
            | txOutAddress o == addr = n + valueOf (txOutValue o) adaSymbol adaToken
            | otherwise              = n

{-# INLINABLE mkWrappedStakeValidator' #-}
mkWrappedStakeValidator' :: PubKeyHash -> Address -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator' pkh addr = wrapStakeValidator $ mkStakeValidator' pkh addr

stakeValidator' :: PubKeyHash -> Address -> StakeValidator
stakeValidator' pkh addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedStakeValidator' ||])
        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode addr

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

pkhFromString :: String -> PubKeyHash
pkhFromString = PubKeyHash . toBuiltin . bytesFromHex . BS8.pack

saveStakeValidator' :: String -> String -> IO ()
saveStakeValidator' pkh bech32 = do
    addr <- case tryReadAddress bech32 of
        Nothing   -> ioError $ userError $ "Invalid address: " <> bech32
        Just addr -> pure addr
    writeStakeValidatorToFile "./assets/staking.plutus" $ stakeValidator' (pkhFromString pkh) addr
