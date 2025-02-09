{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import qualified NFT
import qualified HW.Oracle as Oracle
import           HW.Oracle (OracleDatum(..))
import qualified HW.Collateral as Collateral
import qualified HW.Minting as Minting
import           Control.Monad.State
import           Control.Monad.Trans.Free
import           Plutus.Model           (Ada (Lovelace), DatumMode (..),
                                         Tx, TypedValidator (TypedValidator),
                                         UserSpend, ada, adaValue,
                                         mustFail,
                                         newUser, payToKey, payToScript, spend, submitTx, testNoErrors,
                                         toV2, userSpend,
                                         valueAt, TypedPolicy (TypedPolicy), mintValue, spendPubKey, scriptCurrencySymbol, 
                                          spendScript, refInputInline)
import           Plutus.Model.Contract  (checkErrors, checkBalance, owns, BalanceDiff)
import           Plutus.Model.Mock
import           Plutus.V2.Ledger.Api   (PubKeyHash,
                                         TxOut (txOutValue), TxOutRef, Value, singleton,
                                         TokenName, txOutDatum, OutputDatum (..), fromBuiltinData,
                                         getDatum)
import           PlutusTx.Builtins      (Integer)
import           PlutusTx.Prelude       (Eq ((==)), ($), (.), Maybe (..), negate)
import           Prelude                (IO, mconcat, Semigroup ((<>)), String, pure, maybe, flip, Applicative(..), show, div, (*), mempty, Show, (&&), Ord, uncurry)
import           Test.Tasty             (defaultMain, testGroup, TestTree)
import           Test.Tasty.HUnit
import           Plutus.V1.Ledger.Value (assetClass, AssetClass (), assetClassValue, valueOf, adaSymbol, adaToken)
import           Utilities
import qualified Data.Map as Map

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
    testGroup
      "Testing validator with some sensible values"
      [ good "Minting NFT works               " testMintNFT
      , bad  "Minting the same NFT twice fails" testMintNFTTwice
      , good "Deploying the Oracle works      " testDeployOracle
      , good "Updating the Oracle works       " testUpdateOracle
      , bad  "Bad signer in update            " testUpdateOracleWrongSigner
      , goodSteps "User mints stablecoin      " testMintStableCoin
      , goodSteps "End to end                 " testE2E
      , goodSteps "Liquidation cases          " testLiquidationCases
      ]
    where
      bad msg = good msg . mustFail
      good = testNoErrors (adaValue 10_000_000_000) defaultBabbage
      goodSteps msg = testNoErrorsSteps (adaValue 10_000_000_000) defaultBabbage msg

data RunF f
  = RunFRun f
  | RunFMsg String f
  deriving (Functor)

newtype RunW a = RunW { unRunW :: FreeT RunF Run a}
  deriving (Functor, Applicative, Monad, MonadFail)

stepRun :: Run a -> RunW a
stepRun = RunW . FreeT . fmap Pure

stepMsg :: String -> RunW ()
stepMsg msg = RunW . liftF $ RunFMsg msg ()

withStepLog :: Show a => String -> RunW a -> RunW a
withStepLog prefix wa = do
  a <- wa
  stepMsg $ prefix <> " " <> show a
  pure a

-- | If we want to annotate steps of a test case
testNoErrorsSteps :: Value -> MockConfig -> String -> RunW () -> TestTree
testNoErrorsSteps funds cfg title runw =
  testCaseSteps title \notify -> flip evalStateT (initMock cfg funds) do
    iterT (useNotify (lift . notify)) . hoistFreeT performRun . unRunW $ runw
  where
    useNotify :: Monad m => (String -> m a) -> RunF (m r) -> m r
    useNotify notify = \case
        RunFRun f -> f
        RunFMsg msg f -> notify msg >> f
    performRun :: Run a -> StateT Mock IO a
    performRun run = do
       (merr, a) <- state $ runMock (run >>= \a -> fmap (,a) checkErrors)
       maybe (pure a) (lift . assertFailure) merr

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 4 $ newUser $ ada (Lovelace 1_000_000_000)

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING MINTING NFT -----------------------------------------

-- NFT Minting Policy's script
nftScript :: TxOutRef -> TokenName -> TypedPolicy ()
nftScript ref tn = TypedPolicy . toV2 $ NFT.nftPolicy ref tn

mintNFTTx :: TxOutRef -> TxOut -> TokenName -> Value -> PubKeyHash -> Tx
mintNFTTx ref out tn val pkh =
  mconcat
    [ mintValue (nftScript ref tn) () val
    , payToKey pkh $ val <> txOutValue out
    , spendPubKey ref
    ]

mintNFT :: PubKeyHash -> Run AssetClass
mintNFT u = do
  utxos <- utxoAt u
  let [(ref, out)] = utxos                
      currSymbol = scriptCurrencySymbol (nftScript ref "NFT")
      mintingValue = singleton currSymbol "NFT" 1
  submitTx u $ mintNFTTx ref out "NFT" mintingValue u
  v1<- valueAt u
  unless (v1 == adaValue 1000000000 <> mintingValue) $
    logError "Final balances are incorrect"
  return $ assetClass currSymbol "NFT"

testMintNFT :: Run ()
testMintNFT = do
  [u1,_,_,_] <- setupUsers
  void $ mintNFT u1

testMintNFTTwice :: Run ()
testMintNFTTwice = do
  [u1,_,_,_] <- setupUsers
  utxos <- utxoAt u1               
  let [(ref, out)] = utxos                
      mintingValue = singleton (scriptCurrencySymbol (nftScript ref "NFT")) "NFT" 1
      tx = mintNFTTx ref out "NFT" mintingValue u1
  submitTx u1 tx
  submitTx u1 tx
  v1 <- valueAt u1
  unless (v1 == adaValue 1000000000 <> mintingValue) $
    logError "Final balances are incorrect"

---------------------------------------------------------------------------------------------------
---------------------------------------- TESTING ORACLE -------------------------------------------

type OracleValidator = TypedValidator OracleDatum Oracle.OracleRedeemer

-- Oracle's script
oracleScript :: Oracle.OracleParams -> OracleValidator
oracleScript oracle = TypedValidator . toV2 $ Oracle.validator oracle

deployOracleTx :: UserSpend -> Oracle.OracleParams -> OracleDatum -> Value -> Tx
deployOracleTx sp op dat val =
  mconcat
    [ userSpend sp
    , payToScript (oracleScript op) (InlineDatum dat) (adaValue 1 <> val)
    ]

deployOracle :: PubKeyHash -> OracleDatum-> Run (OracleValidator, AssetClass)
deployOracle u or = do
  -- Mint NFT
  nftAC <- mintNFT u
  -- Deploy Oracle
  let nftV  = assetClassValue nftAC 1
  sp <- spend u $ adaValue 1 <> nftV
  let
      oracle = Oracle.OracleParams nftAC u
      oracleTx = deployOracleTx sp oracle or nftV
  submitTx u oracleTx
  return (oracleScript oracle, nftAC)

testDeployOracle :: Run ()
testDeployOracle = do
  [u1,_,_,_] <- setupUsers
  -- Deploy Oracle
  (ov, _) <- deployOracle u1 $ OracleDatum 25 u1
  -- Check that the oracle deployed correctly
  [(ref,_)] <- utxoAt ov
  dat <- datumAt ref :: Run (Maybe OracleDatum)
  case dat of
    Just d -> unless (oracleDatumRate d == 25
                   && oracleDatumFeeRecipient d == u1
                      ) $ logError "Datum doesn't match!"
    _ -> logError "Oracle is not deployed correctly: Could not find datum"


updateOracleTx :: Oracle.OracleParams -> OracleDatum -> OracleDatum -> TxOutRef -> Value -> Tx
updateOracleTx op last new oRef nft =
  mconcat
    [ spendScript (oracleScript op) oRef Oracle.Update last
    , payToScript (oracleScript op) (InlineDatum new) (adaValue 1 <> nft)
    ]

updateOracle :: PubKeyHash -> OracleDatum ->  OracleDatum -> OracleValidator -> AssetClass -> Run ()
updateOracle u last new ov nftAC = do
  [(ref,_)] <- utxoAt ov
  let oracle = Oracle.OracleParams nftAC u
      oracleTx = updateOracleTx oracle last new ref (assetClassValue nftAC 1)
  signed <- signTx u oracleTx
  submitTx u signed
  --return $ oracleScript oracle

testUpdateOracle :: Run ()
testUpdateOracle = do
  [u1,_,_,_] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle u1 (OracleDatum 25 u1)
  -- Update Oracle
  updateOracle u1 (OracleDatum 25 u1) (OracleDatum 26 u1) ov ac
  -- Check that the oracle updated correctly
  [(ref,_)] <- utxoAt ov
  dat <- datumAt ref :: Run (Maybe OracleDatum)
  case dat of
    Just d -> unless (oracleDatumRate d == 26) $ logError "Datum doesn't match!"
    _ -> logError "Oracle is not deployed correctly: Could not find datum"

testUpdateOracleWrongSigner :: Run ()
testUpdateOracleWrongSigner = do
  [u1,u2,_,_] <- setupUsers
  -- Deploy Oracle
  (ov, ac) <- deployOracle u1 (OracleDatum 25 u1)
  -- Update Oracle
  ov' <-  updateOracleWS u2 u1 (OracleDatum 25 u1) (OracleDatum 26 u1) ov ac
  -- Check that the oracle updated correctly
  [(ref,_)] <- utxoAt ov'
  dat <- datumAt ref :: Run (Maybe OracleDatum)
  case dat of
    Just d -> unless (oracleDatumRate d == 26) $ logError "Datum doesn't match!"
    _ -> logError "Oracle is not deployed correctly: Could not find datum"



updateOracleWS :: PubKeyHash -> PubKeyHash -> OracleDatum ->  OracleDatum -> OracleValidator -> AssetClass -> Run OracleValidator
updateOracleWS signer u last new ov nftAC = do
  [(ref,_)] <- utxoAt ov
  let oracle = Oracle.OracleParams nftAC u
      oracleTx = updateOracleTx oracle last new ref (assetClassValue nftAC 1)
  signed <- signTx signer oracleTx
  submitTx signer oracleTx
  return $ oracleScript oracle


type CollateralValidator = TypedValidator Collateral.CollateralDatum Collateral.CollateralRedeemer

-- Collateral script
collateralScript :: CollateralValidator
collateralScript = TypedValidator . toV2 $ Collateral.validator

-- Stablecoin policy
stableCoinScript :: Minting.MintParams -> TypedPolicy Minting.MintRedeemer
stableCoinScript = TypedPolicy . toV2 . Minting.policy

mintStablecoinTx :: Value
                 -> PubKeyHash
                 -> TypedPolicy Minting.MintRedeemer
                 -> Value
                 -> TxOutRef
                 -> Collateral.CollateralDatum
                 -> UserSpend
                 -> PubKeyHash
                 -> Value
                 -> Tx
mintStablecoinTx col u pol val ref dat us devU devFee = mconcat $
                 [ mintValue pol Minting.Mint val
                 -- , payToKey u val
                 , refInputInline ref
                 , payToScript collateralScript (InlineDatum dat) col
                 , userSpend us
                 -- , payToKey devU devFee
                 ] <> squashAndMap payToKey
                 [ (u, val)
                 , (devU, devFee)
                 ]

burnStablecoinTx :: Value -> UserSpend -> PubKeyHash -> TypedPolicy Minting.MintRedeemer -> TxOutRef -> Collateral.CollateralDatum -> Value -> PubKeyHash -> Value -> Tx
burnStablecoinTx col us user policy ref dat burnVal devU devFee = mconcat $
                   [ spendScript collateralScript ref Collateral.Redeem dat
                   -- , payToKey user col
                   , mintValue policy Minting.Burn (negate burnVal)
                   , userSpend us
                   -- , payToKey devU devFee
                   ] <> squashAndMap payToKey
                   [ (user, col)
                   , (devU, devFee)
                   ]

liquidateStablecoinTx :: Value -> Value -> UserSpend -> PubKeyHash -> PubKeyHash -> TypedPolicy Minting.MintRedeemer -> TxOutRef -> TxOutRef -> Collateral.CollateralDatum -> Value -> PubKeyHash -> Value -> Tx
liquidateStablecoinTx colBorrowed colReturned us user userColl policy oracleRef ref dat burnVal devU devFee = mconcat $
                   [ spendScript collateralScript ref Collateral.Liquidate dat
                   -- , payToKey user colBorrowed
                   -- , payToKey userColl colReturned
                   , refInputInline oracleRef
                   , mintValue policy Minting.Liquidate (negate burnVal)
                   , userSpend us
                   -- , payToKey devU devFee
                   ] <> squashAndMap payToKey
                   [ (user, colBorrowed)
                   , (userColl, colReturned)
                   , (devU, devFee)
                   ]

squashAndMap :: (Ord a, Semigroup b) => (a -> b -> r) -> [(a, b)] -> [r]
squashAndMap f = fmap (uncurry f) . Map.toList . Map.fromListWith (<>)

calcDevFee :: Value -> Value
calcDevFee = withAdaOnly (`div` 1000)

withAdaOnly :: (Integer -> Integer) -> Value -> Value
withAdaOnly f = adaValue . f . valueOfAda
  where
    valueOfAda v = valueOf v adaSymbol adaToken

mintStablecoin :: Value -> TxOutRef -> PubKeyHash -> Value -> Collateral.CollateralDatum -> Oracle.OracleParams -> Run ()
mintStablecoin col oRef user mintingVal datum op = do

  let
      devFee = calcDevFee col
  sp <- spend user $ col <> devFee
  let oracleVH = validatorHash' $ Oracle.validator op

      -- Get Collateral validatorhash
      collateralVH = validatorHash' Collateral.validator
      -- Get Stablecoin minting policy
      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150

      devU = Oracle.oOperator op
      tx = mintStablecoinTx col user stablecoinMP mintingVal oRef datum sp devU devFee
  submitTx user tx

liquidateStablecoin :: Value -> Value -> TxOutRef -> TxOutRef -> PubKeyHash -> PubKeyHash -> Value -> Collateral.CollateralDatum -> Oracle.OracleParams -> Value -> Run ()
liquidateStablecoin col colBorrowed oRef ref user userColl mintingVal datum op devFee = do
  sp <- spend user mintingVal
  let oracleVH = validatorHash' $ Oracle.validator op

      colReturned = col <> negate (colBorrowed <> devFee)

      -- Get Collateral validatorhash
      collateralVH = validatorHash' Collateral.validator
      -- Get Stablecoin minting policy
      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
      devU = Oracle.oOperator op
      tx = liquidateStablecoinTx colBorrowed colReturned sp user userColl stablecoinMP oRef ref datum mintingVal devU devFee
  submitTx user tx

burnStablecoin :: Value -> TxOutRef -> TypedPolicy Minting.MintRedeemer -> Collateral.CollateralDatum -> PubKeyHash -> Value -> PubKeyHash -> Value -> Run ()
burnStablecoin col oRef policy dat user value devU devFee = do
  us <- spend user $ value <> devFee
  let tx = burnStablecoinTx col us user policy oRef dat value devU devFee
  submitTx user tx

checkBalance_ :: BalanceDiff -> Run a -> Run a
checkBalance_ = checkBalance
-- checkBalance_ _ a = a

testE2E :: RunW ()
testE2E = do
  u1:u2:dev:_ <- stepRun setupUsers
  stepMsg "Deploy Oracle"
  (ov, ac) <- stepRun $ deployOracle dev (OracleDatum 200 dev)
  let amountToMint = 2
      oracleParams = Oracle.OracleParams ac dev
      oracleVH     = validatorHash' $ Oracle.validator oracleParams
      collateralVH = validatorHash' Collateral.validator

      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
      currSymbol = scriptCurrencySymbol stablecoinMP
      stableCoinValue = singleton currSymbol Collateral.stablecoinTokenName
      datumU1 = Collateral.CollateralDatum currSymbol u1 amountToMint dev
      datumU2 = Collateral.CollateralDatum currSymbol u2 amountToMint dev
      mintingValue = stableCoinValue amountToMint
      collateral = adaValue 3000000
      colBorrowed = withAdaOnly (\a -> a * 2 `div` 100) collateral
      colReturned = collateral <> negate colBorrowed <> negate (calcDevFee collateral)

  stepMsg "Update Oracle"
  stepRun $ updateOracle dev (OracleDatum 200 dev) (OracleDatum 100 dev) ov ac
  [(ref,_)] <- stepRun $ utxoAt ov
  stepMsg "Mint stablecoin"
  stepRun $ mintStablecoin collateral ref u1 mintingValue datumU1 oracleParams
  [u1Collateral] <-
      -- withStepLog "Collateral for u1" $
      stepRun $ findCollateralFor u1
  stepMsg "Burn stablecoin"
  stepRun $ burnStablecoin collateral u1Collateral stablecoinMP datumU1 u1 mintingValue dev (calcDevFee collateral)
  stepMsg "Liquidate stablecoin"
  stepRun $ mintStablecoin collateral ref u1 mintingValue datumU1 oracleParams
  stepRun $ checkBalance_ (owns dev (calcDevFee collateral)) $
    mintStablecoin collateral ref u2 mintingValue datumU2 oracleParams
  [u2Collateral] <-
      -- withStepLog "Collateral for u2" $
      stepRun $ findCollateralFor u2
  stepMsg "Update Oracle"
  stepRun $ updateOracle dev (OracleDatum 200 dev) (OracleDatum 50 dev) ov ac
  [(ref',_)] <- stepRun $ utxoAt ov
  stepMsg "Tries to burn stablecoin with paying the developer less than necessary and fails"
  stepRun $ mustFail $ burnStablecoin collateral u2Collateral stablecoinMP datumU2 u2 mintingValue dev (withAdaOnly (`div` 2) (calcDevFee collateral))
  stepMsg "Liquidate and check"
  let devFee = calcDevFee collateral
  stepRun $ checkBalance_ (
      owns u1 (stableCoinValue (-2))
      <> owns u1 colBorrowed
      <> owns u2 colReturned
      <> owns dev devFee
    ) $
    liquidateStablecoin collateral colBorrowed ref' u2Collateral u1 u2 mintingValue datumU2 oracleParams devFee

testLiquidationCases :: RunW ()
testLiquidationCases = do
  [u1,u2,u3,owner] <- stepRun $ setupUsers
  stepMsg "Deploy Oracle"
  (ov, ac) <- stepRun $ deployOracle owner (OracleDatum 100 owner)
  let amountToMintU1 = 2
      amountToMintU2 = 4
      amountToMintU3 = 4
      oracleParams = Oracle.OracleParams ac owner
      oracleVH     = validatorHash' $ Oracle.validator oracleParams
      collateralVH = validatorHash' Collateral.validator

      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
      currSymbol = scriptCurrencySymbol stablecoinMP
      stableCoinValue = singleton currSymbol Collateral.stablecoinTokenName
      datumU1 = Collateral.CollateralDatum currSymbol u1 amountToMintU1 owner
      datumU2 = Collateral.CollateralDatum currSymbol u2 amountToMintU2 owner
      datumU3 = Collateral.CollateralDatum currSymbol u3 amountToMintU3 owner
      mintingValueU1 = stableCoinValue amountToMintU1
      mintingValueU2 = stableCoinValue amountToMintU2
      mintingValueU3 = stableCoinValue amountToMintU3

      collateral1Int = 6000000
      collateral1 = adaValue collateral1Int
      collateral1liqBorrowed = adaValue $ collateral1Int * 2 `div` 100

      collateral2Int = 6000000
      collateral2 = adaValue collateral2Int
      collateral2liqBorrowed = adaValue $ collateral2Int * 2 `div` 100

      collateral3 = adaValue 8000000

  [(ref,_)] <- stepRun $ utxoAt ov

  stepMsg "Users 1 2 and 3 mint stablecoin"
  stepRun do
    mintStablecoin collateral1 ref u1 mintingValueU1 datumU1 oracleParams
    mintStablecoin collateral2 ref u2 mintingValueU2 datumU2 oracleParams
    mintStablecoin collateral3 ref u3 mintingValueU3 datumU3 oracleParams

  stepMsg "Owner updates the Oracle"
  stepRun $ updateOracle owner (OracleDatum 100 owner) (OracleDatum 50 owner) ov ac
  [(ref',_)] <- stepRun $ utxoAt ov
  [u1Collateral] <- stepRun $ findCollateralFor u1
  [u2Collateral] <- stepRun $ findCollateralFor u2

  stepMsg "User3 tries to liquidate collateral of user 1 but fails"
  stepRun $ mustFail $ liquidateStablecoin collateral1 collateral1liqBorrowed ref' u1Collateral u3 u1 mintingValueU1 datumU1 oracleParams (calcDevFee collateral1)

  stepMsg "User3 tries to liquidate collateral of user2, get more than 2%, and fails"
  stepRun $ mustFail $
    liquidateStablecoin collateral2 (withAdaOnly (* 2) collateral2liqBorrowed)
                        ref' u2Collateral u3 u2 mintingValueU2 datumU2 oracleParams (calcDevFee collateral2)

  stepMsg "User3 tries to liquidate collateral of user2 and succeeds"
  -- stepMsg . ("u2 before liquidation: " <>) . show =<< stepRun (valueAt u2)
  -- stepMsg . ("u3 before liquidation of u2: " <>) . show =<< stepRun (valueAt u3)
  stepRun do
    let devFee = calcDevFee collateral2
    checkBalance_ (
          owns u3 (stableCoinValue (-4))
          <> owns u3 collateral2liqBorrowed
          <> owns u2 (collateral2 <> negate collateral2liqBorrowed <> negate devFee)
          <> owns owner devFee
        ) $
        liquidateStablecoin collateral2 collateral2liqBorrowed ref' u2Collateral u3 u2 mintingValueU2 datumU2 oracleParams devFee
  -- stepMsg . ("u2  after liquidation: " <>) . show =<< stepRun (valueAt u2)
  -- stepMsg . ("u3  after liquidation of u2: " <>) . show =<< stepRun (valueAt u3)

findCollateralFor :: PubKeyHash -> Run [TxOutRef]
findCollateralFor user = do
  utxos <- utxoAt collateralScript
  let refs' = [ ref | (ref,o) <- utxos, getOwner o == Just user]
  return refs'

getOwner :: TxOut -> Maybe PubKeyHash
getOwner oRef = case txOutDatum oRef of
   OutputDatum od -> case fromBuiltinData (getDatum od) of
     Nothing              -> Nothing
     Just collateralDatum -> Just $ Collateral.colOwner collateralDatum
   _ -> Nothing

testMintStableCoin :: RunW ()
testMintStableCoin = do
  u1:u2:dev:_ <- stepRun setupUsers
  -- Deploy Oracle
  (ov, ac) <- stepRun $ deployOracle dev (OracleDatum 200 dev)
  -- Update Oracle
  stepRun $ updateOracle dev (OracleDatum 200 dev) (OracleDatum 100 dev) ov ac
  [(ref,_)] <- stepRun $ utxoAt ov

  let

      -- get Oracle validatorHash
      oracleVH = validatorHash' $ Oracle.validator $ Oracle.OracleParams ac dev

      -- get Collateral validatorhash
      collateralVH = validatorHash' Collateral.validator

      stablecoinMP = stableCoinScript $ Minting.MintParams oracleVH collateralVH 150
      currSymbol = scriptCurrencySymbol stablecoinMP
      mintingValue = singleton currSymbol Collateral.stablecoinTokenName 2

      collateral = adaValue 3000000
      devFee = calcDevFee collateral

      mkTx u sp fee = do
        let datum = Collateral.CollateralDatum currSymbol u 2 dev
        mintStablecoinTx collateral u stablecoinMP mintingValue ref datum sp dev fee

  stepMsg "User mints a stablecoin"
  sp2 <- stepRun $ spend u1 (collateral <> devFee)
  stepRun $ checkBalance_ (
                owns u1 (negate (collateral <> devFee))
             <> owns u1 mintingValue
             <> owns dev devFee
    ) $
    submitTx u1 (mkTx u1 sp2 devFee)

  stepMsg "User tries to mint a stablecoin without a developer fee and fails"
  sp3 <- stepRun $ spend u1 collateral
  stepRun $ mustFail $ submitTx u1 (mkTx u1 sp3 mempty)

  stepMsg "User tries to mint a stablecoin with a wrong developer fee address and fails"
  stepRun do
    sp <- spend u1 (collateral <> devFee)
    let datum = Collateral.CollateralDatum currSymbol u2 2 u2
        tx = mintStablecoinTx collateral u1 stablecoinMP mintingValue ref datum sp dev devFee
    -- Must fail with validation error: "invalid datum at collateral output"
    mustFail $ submitTx u1 tx

