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

module Week05.Signed where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
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
import Prelude (IO, Show (..), String)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` (PlutusTx.liftCode pkh)

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

data MintParams = MintParams
  { mpTokenName :: !TokenName
  , mpAmount :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  let val = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
      lookups = Constraints.mintingPolicy $ policy pkh
      tx = Constraints.mustMintValue val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
  let tn = "ABC"
  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints
  callEndpoint @"mint" h1 $
    MintParams
      { mpTokenName = tn
      , mpAmount = 555
      }
  callEndpoint @"mint" h2 $
    MintParams
      { mpTokenName = tn
      , mpAmount = 444
      }
  void $ Emulator.waitNSlots 1
  callEndpoint @"mint" h1 $
    MintParams
      { mpTokenName = tn
      , mpAmount = -222
      }
  void $ Emulator.waitNSlots 1
