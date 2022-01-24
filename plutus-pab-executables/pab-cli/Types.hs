{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Exception (Exception)
import Plutus.ChainIndex.Types (Point)

data AppOpts =
    AppOpts { appOptsCommand        :: ConfigCommand
            , appOptsNodeOpts       :: NodeOpts
            , appOptsChainIndexOpts :: ChainIndexOpts
            } deriving (Show)

data PABOpts =
    PABOpts { pabOptsExe             :: PABExe
            , pabOptsOutputDir       :: PABDirectory
            , pabOptsPort            :: PABPort
            , pabOptsDbPoolSize      :: Int
            , pabOptsRollbackHistory :: Maybe Int
            , pabOptsPassphrase      :: Maybe String
            , pabOptsResumeFromPoint :: Point
            } deriving (Show)

newtype ChainIndexOpts =
    ChainIndexOpts { chainIndexOptsPort :: ChainIndexPort
                   } deriving (Show)

data NodeOpts =
    NodeOpts { nodeOptsOutputDir :: NodeDirectory
             , nodeOptsPort      :: NodePort
             } deriving (Show)

data ConfigCommand =
    MockNetCommand PABOpts WalletPort
  | NodeWBECommand NetworkName (Maybe PABOpts) WalletPort
  | NodeRemoteWalletCommand NetworkName (Maybe PABOpts)
    deriving (Show)

newtype PABExe = PABExe { unPABExe :: FilePath }
    deriving (Show)

newtype NodeDirectory = NodeDirectory { unNodeDirectory :: FilePath }
    deriving (Show)

newtype NodeSocketPath = NodeSocketPath { unNodeSocketPath :: FilePath }
    deriving (Show)

newtype NodeDbPath = NodeDbPath { unNodeDbPath :: FilePath }
    deriving (Show)

newtype NodePort = NodePort { unNodePort :: Int }
    deriving (Show)

newtype PABPort = PABPort { unPABPort :: Int }
    deriving (Show)

newtype PABDirectory = PABDirectory { unPABDirectory :: FilePath }
    deriving (Show)

newtype PABDbPath = PABDbPath { unPABDbPath :: FilePath }
    deriving (Show)

-- The show instance should output 'Testnet' and 'Mainnet'. Used in 'fetchNodeConfigFiles'.
data NetworkName = Testnet | Mainnet
    deriving (Show)

newtype ChainIndexPort = ChainIndexPort { unChainIndexPort :: Int }
    deriving (Show)

newtype ChainIndexDbPath = ChainIndexDbPath { unChainIndexDbPath :: FilePath }
    deriving (Show)

newtype WalletPort = WalletPort { unWalletPort :: Int }
    deriving (Show)

newtype WBEDbDirectory = WBEDbDirectory { unWBEDbDirectory :: FilePath }
    deriving (Show)

data AppError = ChainIndexPortError
              | PABPortError
              | PABExeNotProvidedError
              | PABExeNotExecutableError
              | PABDBPoolSizeError
              | PABRollbackHistoryNotNumberError
              | PABResumeFromBlockIdNotSpecifiedError
              | PABResumeFromSlotNotSpecifiedError
              | PABResumeFromSlotNotANumberError
              | NodePortError
              | UnspecifiedNodeNetworkError
              | WalletPortError

instance Exception AppError

instance Show AppError where
    show ChainIndexPortError = "Option --chain-index-port should be a positive number"
    show PABPortError = "Option --pab-port should be a positive number"
    show PABExeNotProvidedError = "Executable specified in --pab-exe is not available in the current PATH"
    show PABExeNotExecutableError = "Current user does not have executable permissions for the executable specified in --pab-exe."
    show PABDBPoolSizeError = "Option --pab-db-pool-size should be a positive number"

    show PABRollbackHistoryNotNumberError = "Option --pab-rollback-history is not a number"
    show PABResumeFromBlockIdNotSpecifiedError = "Option --pab-resume-from-block-id is not specified"
    show PABResumeFromSlotNotSpecifiedError = "Option --pab-resume-from-slot is not specified"
    show PABResumeFromSlotNotANumberError = "Option --pab-resume-from-slot should be positive number"

    show NodePortError = "Option --node-port should be a positive number"
    show UnspecifiedNodeNetworkError = "Unspecified cardano network"
    show WalletPortError = "Option --wallet-port should be a positive number"
