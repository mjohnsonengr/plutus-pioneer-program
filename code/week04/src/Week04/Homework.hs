{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Control.Monad.Freer.Extras as Extras
import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet
import Data.Void (Void)

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Void ()
payContract = do
    void $ Contract.handleError 
        (\err -> Contract.logError $ "caught: " ++ unpack err)
        payContract'
    payContract

payContract' :: Contract () PaySchema Text ()
payContract' = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace t1 t2 = do
    h1 <- activateContractWallet (Wallet 1) payContract
    -- void $ activateContractWallet (Wallet 2) payContract
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace  = t1
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace  = t2
        }
    s <- Emulator.waitNSlots 1
    Extras.logInfo $ "reached " ++ show s


payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
