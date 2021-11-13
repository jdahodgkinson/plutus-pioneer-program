{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Week05.Homework2 where

import Control.Monad hiding (fmap)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)
import Ledger hiding (mint, singleton)
import Ledger.Constraints as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value as Value
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract as Contract
import Plutus.Trace.Emulator as Emulator
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Wallet.Emulator.Wallet
import Prelude (IO, Semigroup (..), Show (..), String, undefined)

{-# INLINEABLE tn #-}
tn :: TokenName
tn = TokenName emptyByteString

{- | Minting policy for an NFT, where the minting transaction
    must consume the given UTxO as input and where the TokenName
    will be the empty ByteString.
-}
{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs txInfo

    checkMintedAmount :: Bool
    checkMintedAmount =
      case flattenValue (txInfoForge txInfo) of
        [(cs, tn', amt)] ->
          cs == ownCurrencySymbol ctx
            && tn' == tn
            && amt == 1
        _ -> False

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

type NFTSchema = Endpoint "mint" ()

mint :: Contract w NFTSchema Text ()
mint = do
  pk <- Contract.ownPubKey
  utxos <- utxoAt (pubKeyAddress pk)
  case Map.keys utxos of
    [] -> Contract.logError @String "no utxos available"
    oref : _ -> do
      let val = Value.singleton (curSymbol oref) tn 1
          lookups =
            Constraints.mintingPolicy (policy oref)
              <> Constraints.unspentOutputs utxos
          tx =
            Constraints.mustMintValue val
              <> Constraints.mustSpendPubKeyOutput oref
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >> mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints
  callEndpoint @"mint" h1 ()
  callEndpoint @"mint" h2 ()
  void $ Emulator.waitNSlots 1
