{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V1.Ledger.Value     (flattenValue, adaToken)
import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy,
                                       ScriptContext (scriptContextTxInfo),
                                       TxInfo (txInfoInputs, txInfoMint),
                                       TxOutRef,
                                       TxInInfo (txInInfoOutRef),
                                       mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), traceIfFalse, any,
                                             Eq ((==)), ($), (&&))
import           Utilities            (wrapPolicy)



{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkEmptyNFTPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkEmptyNFTPolicy oref () ctx =
       traceIfFalse "UTxO not consumed"   hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedNameAndAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedNameAndAmount :: Bool
    checkMintedNameAndAmount = case flattenValue (txInfoMint info) of
        [(_, name, amount)] -> name == adaToken && amount == 1
        _                   -> False

-- {-# INLINABLE mkWrappedEmptyNFTPolicy #-}
-- mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
-- mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

-- nftPolicy :: TxOutRef -> MintingPolicy
-- nftPolicy oref = mkMintingPolicyScript $
--    $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||])
--       `PlutusTx.applyCode` PlutusTx.liftCode oref

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy oref = wrapPolicy $
  mkEmptyNFTPolicy (PlutusTx.unsafeFromBuiltinData oref)

nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||])

nftPolicy :: TxOutRef -> MintingPolicy
nftPolicy oref = mkMintingPolicyScript $
   nftCode
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData oref)

