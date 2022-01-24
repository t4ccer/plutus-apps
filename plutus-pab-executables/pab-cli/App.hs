{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module App where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (waitAny, withAsync)
import Control.Monad (forM_, unless, void)
import Control.Monad.Catch (catch)
import Control.Monad.Freer (Eff, LastMember, Member)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Default (def)
import Data.Text qualified as Text
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Yaml (encodeFile)
import Network.HTTP.Req (GET (GET), NoReqBody (NoReqBody), bsResponse, defaultHttpConfig, https, req, responseBody,
                         runReq, (/:))
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Process (callCommand)

import Cardano.Api qualified as C
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (NetworkIdWrapper, unNetworkIdWrapper))
import Cardano.ChainIndex.Types (ChainIndexConfig (ciBaseUrl), ChainIndexUrl (ChainIndexUrl))
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (sgNetworkMagic, sgSecurityParam, sgSlotLength, sgSystemStart))
import Cardano.Node.Types (NodeMode (AlonzoNode),
                           PABServerConfig (pscBaseUrl, pscKeptBlocks, pscNetworkId, pscNodeMode, pscSlotConfig, pscSocketPath))
import Cardano.Wallet.Types (LocalWalletSettings (LocalWalletSettings),
                             WalletConfig (LocalWalletConfig, RemoteWalletConfig), WalletUrl (WalletUrl))
import Ledger (POSIXTime (POSIXTime))
import Ledger.TimeSlot (SlotConfig (SlotConfig))
import Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import Plutus.PAB.Types (Config (chainIndexConfig, dbConfig, developmentOptions, nodeServerConfig, pabWebserverConfig, walletServerConfig),
                         DbConfig (dbConfigFile, dbConfigPoolSize),
                         DevelopmentOptions (DevelopmentOptions, pabResumeFrom, pabRollbackHistory),
                         WebserverConfig (baseUrl))

import Types (AppOpts (AppOpts, appOptsChainIndexOpts, appOptsCommand, appOptsNodeOpts),
              ChainIndexDbPath (ChainIndexDbPath, unChainIndexDbPath),
              ChainIndexOpts (ChainIndexOpts, chainIndexOptsPort), ChainIndexPort (ChainIndexPort, unChainIndexPort),
              ConfigCommand (MockNetCommand, NodeRemoteWalletCommand, NodeWBECommand), NetworkName (Mainnet, Testnet),
              NodeDbPath (NodeDbPath, unNodeDbPath), NodeDirectory (NodeDirectory, unNodeDirectory),
              NodeOpts (NodeOpts, nodeOptsOutputDir, nodeOptsPort), NodePort (NodePort, unNodePort),
              NodeSocketPath (NodeSocketPath, unNodeSocketPath), PABDbPath (PABDbPath),
              PABDirectory (PABDirectory, unPABDirectory), PABExe (unPABExe),
              PABOpts (PABOpts, pabOptsDbPoolSize, pabOptsExe, pabOptsOutputDir, pabOptsPassphrase, pabOptsPort, pabOptsResumeFromPoint, pabOptsRollbackHistory),
              PABPort (PABPort), WBEDbDirectory (WBEDbDirectory), WalletPort (WalletPort))

cardanoNodeCmd :: String
cardanoNodeCmd = "cardano-node"

cardanoWalletCmd :: String
cardanoWalletCmd = "cardano-wallet"

plutusChainIndexCmd :: String
plutusChainIndexCmd = "plutus-chain-index"

-- | These file names come from: https://hydra.iohk.io/build/7654130/download/1/index.html
nodeConfigFilenames :: [String]
nodeConfigFilenames =
    [ "config.json"
    , "byron-genesis.json"
    , "shelley-genesis.json"
    , "alonzo-genesis.json"
    , "topology.json"
    ]

nodeSocketPath :: NodeDirectory -> NodeSocketPath
nodeSocketPath (NodeDirectory nodeDir) = NodeSocketPath $ nodeDir </> "node.sock"

pabDbPath :: PABDirectory -> PABDbPath
pabDbPath (PABDirectory pabDir) = PABDbPath $ pabDir </> "pab-core.db"

nodeDbPath :: NodeDirectory -> NodeDbPath
nodeDbPath (NodeDirectory nodeDir) = NodeDbPath $ nodeDir </> "db"

chainIndexDbPath :: NodeDirectory -> ChainIndexDbPath
chainIndexDbPath (NodeDirectory nodeDir) = ChainIndexDbPath $ nodeDir </> "chain-index.db"

wbeDatabaseDir :: NodeDirectory -> WBEDbDirectory
wbeDatabaseDir (NodeDirectory nodeDir) = WBEDbDirectory $ nodeDir </> "wbe"

runApp ::
    ( Member (Reader AppOpts) effs
    , MonadIO m
    , LastMember m effs
    )
    => Eff effs ()
runApp = do
    appOpts <- ask @AppOpts

    -- Create node base output directory
    let nodeDir@(NodeDirectory nd) = nodeOptsOutputDir $ appOptsNodeOpts appOpts
    liftIO $ createDirectoryIfMissing True nd

    -- Remote old node socket file
    let NodeSocketPath oldSocketPath = nodeSocketPath nodeDir
    -- All kinds of possible exceptions here (isDoesNotExistError, isPermissionError, etc.)
    liftIO $ catch (removeFile oldSocketPath) $ \(_ :: IOError) -> pure ()

    runAppCommand (appOptsCommand appOpts)

runAppCommand ::
    ( Member (Reader AppOpts) effs
    , MonadIO m
    , LastMember m effs
    )
    => ConfigCommand
    -> Eff effs ()
runAppCommand (MockNetCommand pabOpts (WalletPort walletPort)) = do
    AppOpts { appOptsNodeOpts = NodeOpts { nodeOptsOutputDir
                                         , nodeOptsPort = NodePort nodePort
                                         }
            , appOptsChainIndexOpts
            } <- ask @AppOpts

    let nodeServerConfig =
            def { pscBaseUrl = BaseUrl Http "localhost" nodePort ""
                , pscSocketPath = unNodeSocketPath $ nodeSocketPath nodeOptsOutputDir
                }
        walletConfig = LocalWalletConfig
                     $ LocalWalletSettings
                     $ WalletUrl
                     $ BaseUrl Http "localhost" walletPort ""
        pabServerConfig =
            (pabWebserverBaseConfig nodeServerConfig pabOpts appOptsChainIndexOpts)
                { walletServerConfig = walletConfig
                , nodeServerConfig = nodeServerConfig
                }

    liftIO $ startPabWebserverAndMockServers pabOpts pabServerConfig

runAppCommand (NodeWBECommand networkName pabOptsM walletPort@(WalletPort wp)) = do
    appOpts@AppOpts { appOptsNodeOpts = NodeOpts { nodeOptsOutputDir }
            , appOptsChainIndexOpts
            } <- ask @AppOpts
    -- Fetch node config files depending on network and save to node output directory.
    liftIO $ fetchNodeConfigFiles networkName nodeOptsOutputDir

    nodeServerConfig <- getCardanoNodeConfig networkName nodeOptsOutputDir
    let networkId = unNetworkIdWrapper $ pscNetworkId nodeServerConfig
    let startChainIndexAction =
            startChainIndex networkId appOptsChainIndexOpts (appOptsNodeOpts appOpts)
        startCardanoNodeAction = startCardanoNode networkName (appOptsNodeOpts appOpts)
        startCardanoWalletAction = startCardanoWallet networkName walletPort nodeOptsOutputDir

    let walletConfig = LocalWalletConfig
                     $ LocalWalletSettings
                     $ WalletUrl
                     $ BaseUrl Http "localhost" wp ""
    case pabOptsM of
        Nothing ->
            liftIO $ runAsyncActions [ startChainIndexAction
                                     , startCardanoNodeAction
                                     , startCardanoWalletAction
                                     ]
        Just pabOpts -> do
            -- Generate base PAB configuration and modify the node and wallet
            -- server config
            let pabServerConfig = (pabWebserverBaseConfig nodeServerConfig pabOpts appOptsChainIndexOpts)
                    { walletServerConfig = walletConfig
                    , nodeServerConfig = nodeServerConfig
                    }

            let startPabWebserverAction = startPabWebserver pabOpts pabServerConfig

            liftIO $ runAsyncActions [ startChainIndexAction
                                     , startCardanoNodeAction
                                     , startPabWebserverAction
                                     , startCardanoWalletAction
                                     ]

runAppCommand (NodeRemoteWalletCommand network pabOptsM) = do
    appOpts@AppOpts { appOptsNodeOpts = NodeOpts { nodeOptsOutputDir }
            , appOptsChainIndexOpts
            } <- ask @AppOpts
    -- Fetch node config files depending on network and save to node output directory.
    liftIO $ fetchNodeConfigFiles network nodeOptsOutputDir

    nodeServerConfig <- getCardanoNodeConfig network nodeOptsOutputDir
    let networkId = unNetworkIdWrapper $ pscNetworkId nodeServerConfig
        networkName = getNetworkName networkId
        startChainIndexAction =
            startChainIndex networkId appOptsChainIndexOpts (appOptsNodeOpts appOpts)
        startCardanoNodeAction = startCardanoNode networkName (appOptsNodeOpts appOpts)

    case pabOptsM of
        Nothing ->
            liftIO $ runAsyncActions [ startChainIndexAction
                                     , startCardanoNodeAction
                                     ]
        Just pabOpts -> do
            let pabServerConfig = (pabWebserverBaseConfig nodeServerConfig pabOpts appOptsChainIndexOpts)
                    { walletServerConfig = RemoteWalletConfig
                    , nodeServerConfig = nodeServerConfig
                    }

            let startPabWebserverAction = startPabWebserver pabOpts pabServerConfig

            liftIO $ runAsyncActions [ startChainIndexAction
                                     , startCardanoNodeAction
                                     , startPabWebserverAction
                                     ]

-- | Run list of actions asynchroniously and wait until any one of them finishes.
runAsyncActions :: [IO ()] -> IO ()
-- runAsyncActions ios = do
--     asyncs <- mapM (\io -> withAsync io pure) ios
--     print $ length asyncs
--     void $ waitAny asyncs
runAsyncActions ios = void $ go ios []
    where go [] asyncs = waitAny asyncs
          go (x:xs) asyncs = do
              withAsync x $ \a -> go xs (a : asyncs)

-- | Fetch cardano node config files from Hydra CI given a network.
fetchNodeConfigFiles
    :: NetworkName -- ^ The supported Cardano networks: Mainnet and Testnet
    -> NodeDirectory -- ^ Directory to store the config files
    -> IO ()
fetchNodeConfigFiles networkName (NodeDirectory nodeDir) = do
    forM_ nodeConfigFilenames $ \partialFname -> do
        let configFname = map toLower (show networkName) <> "-" <> partialFname
            configFpath = nodeDir </> configFname
        configFpathExists <- doesFileExist configFpath
        unless configFpathExists $ do
            r <- runReq defaultHttpConfig $ req GET
                     (    https
                          "hydra.iohk.io"
                       /: "build"
                       /: "7654130"
                       /: "download"
                       /: "1"
                       /: Text.pack configFname
                     )
                     NoReqBody
                     bsResponse
                     mempty
            BS.writeFile configFpath $ responseBody r

-- | Generates a base PAB configuration with sensible default values.
pabWebserverBaseConfig
    :: PABServerConfig -- ^ Node configuration inside the PAB configuration
    -> PABOpts -- ^ PAB command line arguments
    -> ChainIndexOpts -- ^ Chain index command line arguments
    -> Config -- ^ PAB configuration
pabWebserverBaseConfig
        nodeServerConfig
        PABOpts { pabOptsOutputDir
                , pabOptsPort = PABPort pabPort
                , pabOptsDbPoolSize
                , pabOptsRollbackHistory
                , pabOptsResumeFromPoint
                }
        ChainIndexOpts { chainIndexOptsPort = ChainIndexPort chainIndexPort } = do
    let PABDbPath dbConfigFile = pabDbPath pabOptsOutputDir
        chainIndexUrl = ChainIndexUrl $ BaseUrl Http "localhost" chainIndexPort ""
        pabWebserverConfig =
            def { baseUrl = BaseUrl Http "localhost" pabPort "" }
    def { dbConfig = def { dbConfigFile = Text.pack dbConfigFile
                         , dbConfigPoolSize = pabOptsDbPoolSize
                         }
        , chainIndexConfig = def { ciBaseUrl = chainIndexUrl }
        , nodeServerConfig = nodeServerConfig
        , pabWebserverConfig = pabWebserverConfig
        , developmentOptions =
            DevelopmentOptions
                { pabRollbackHistory = pabOptsRollbackHistory
                , pabResumeFrom = pabOptsResumeFromPoint
                }
        }

-- | Starts the PAB webserver.
--
-- The server won't start until the socket file is available.
startPabWebserver
    :: PABOpts -- ^ PAB command line arguments
    -> Config -- ^ PAB configuration
    -> IO ()
startPabWebserver PABOpts { pabOptsOutputDir, pabOptsExe, pabOptsPassphrase } pabServerConfig = do
    liftIO $ waitForNodeSocket
        (NodeSocketPath $ pscSocketPath $ nodeServerConfig pabServerConfig)

    let pabDir = unPABDirectory pabOptsOutputDir
    liftIO $ createDirectoryIfMissing True pabDir

    let pabConfigFpath = pabDir </> "plutus-pab.yaml"
    encodeFile pabConfigFpath pabServerConfig

    let passphraseCliOption = maybe "" (" --passphrase " <>) pabOptsPassphrase
    callCommand $ unPABExe pabOptsExe <> " migrate --config " <> pabConfigFpath
    callCommand $ unPABExe pabOptsExe <> " webserver --config " <> pabConfigFpath
                                      <> passphraseCliOption

-- | Starts the PAB webserver.
--
-- The server won't start until the socket file is available.
startPabWebserverAndMockServers
    :: PABOpts -- ^ PAB command line arguments
    -> Config -- ^ PAB configuration
    -> IO ()
startPabWebserverAndMockServers
  PABOpts { pabOptsOutputDir, pabOptsExe } pabServerConfig = do
    let pabDir = unPABDirectory pabOptsOutputDir
    liftIO $ createDirectoryIfMissing True pabDir

    let pabConfigFpath = pabDir </> "plutus-pab.yaml"
    encodeFile pabConfigFpath pabServerConfig

    callCommand $ unPABExe pabOptsExe <> " migrate --config " <> pabConfigFpath
    callCommand $ unPABExe pabOptsExe <> " all-servers --config " <> pabConfigFpath

-- | Starts the 'cardano-node'.
startCardanoNode
    :: NetworkName -- ^ The supported Cardano networks: Mainnet and Testnet
    -> NodeOpts -- ^ Node command line arguments
    -> IO ()
startCardanoNode networkName NodeOpts { nodeOptsOutputDir, nodeOptsPort } = do
    let nodeConfigFpath = unNodeDirectory nodeOptsOutputDir
                      </> map toLower (show networkName)
                      <> "-config.json"
        topologyFpath = unNodeDirectory nodeOptsOutputDir
                    </> map toLower (show networkName)
                    <> "-topology.json"
        databaseFpath = unNodeDbPath (nodeDbPath nodeOptsOutputDir)
        socketPath = unNodeSocketPath (nodeSocketPath nodeOptsOutputDir)
    callCommand $ cardanoNodeCmd
               <> " run"
               <> " --config " <> nodeConfigFpath
               <> " --topology " <> topologyFpath
               <> " --database-path " <> databaseFpath
               <> " --socket-path " <> socketPath
               <> " --port " <> show (unNodePort nodeOptsPort)

-- | Starts the 'chain-index' server
--
-- The server won't start until the socket file is available.
startChainIndex
    :: C.NetworkId -- ^ The supported Cardano networks: Mainnet and Testnet
    -> ChainIndexOpts -- ^ Chain index command line arguments
    -> NodeOpts -- ^ Node command line arguments
    -> IO ()
startChainIndex networkId
                ChainIndexOpts { chainIndexOptsPort }
                NodeOpts { nodeOptsOutputDir } = do
    let socketPath = nodeSocketPath nodeOptsOutputDir
    waitForNodeSocket socketPath
    callCommand $ plutusChainIndexCmd
               <> " --socket-path " <> unNodeSocketPath socketPath
               <> " --db-path " <> unChainIndexDbPath (chainIndexDbPath nodeOptsOutputDir)
               <> " --port " <> show (unChainIndexPort chainIndexOptsPort)
               <> " --network-id " <> show (C.unNetworkMagic $ C.toNetworkMagic networkId)
               <> " start-index"

-- | Starts the `cardano-wallet` server from a specific network (mainnet or
-- testnet) given the node directory which contains the socket file and config
-- files.
--
-- The server won't start until the socket file is available.
startCardanoWallet
    :: NetworkName -- ^ The supported Cardano networks: Mainnet and Testnet
    -> WalletPort -- ^ Port number
    -> NodeDirectory
    -- ^ Directory containing the `cardano node` socket file and config files
    -> IO ()
startCardanoWallet network (WalletPort walletPort) nodeDir@(NodeDirectory nd) = do
    waitForNodeSocket $ nodeSocketPath nodeDir

    let WBEDbDirectory wbeDbDir = wbeDatabaseDir nodeDir
    createDirectoryIfMissing True wbeDbDir
    case network of
        Mainnet ->
            callCommand $ cardanoWalletCmd
                       <> " serve"
                       <> " --mainnet"
                       <> " --node-socket " <> unNodeSocketPath (nodeSocketPath nodeDir)
                       <> " --database " <> wbeDbDir
                       <> " --port " <> show walletPort
        Testnet -> do
            let byronGenesisFilePath = nd
                                   </> fmap toLower (show network)
                                    <> "-byron-genesis.json"
            callCommand $ cardanoWalletCmd
                       <> " serve"
                       <> " --testnet " <> byronGenesisFilePath
                       <> " --node-socket " <> unNodeSocketPath (nodeSocketPath nodeDir)
                       <> " --database " <> wbeDbDir
                       <> " --port " <> show walletPort

-- | Waits until the given node socket path finally exists by polling the
-- filesystem.
waitForNodeSocket :: NodeSocketPath -> IO ()
waitForNodeSocket (NodeSocketPath np) = go
  where
      go = do
          nodeSocketExists <- doesFileExist np
          unless nodeSocketExists $ threadDelay 5000000 >> go

-- | Creates a node config for the PAB configuration using the values in the
-- '(mainnet|testnet)-shelley-genesis.json' file in the given node directory.
--
-- The name is misleading, but 'PABServerConfig' actually refers to the cardano
-- node configuration in the PAB. Needs to be changed.
--
-- A precondition of this function is that the cardano node config files were
-- successfully fetched and put in the given node directory.
getCardanoNodeConfig
    :: (MonadIO m)
    => NetworkName -- ^ The supported Cardano networks: Mainnet and Testnet
    -> NodeDirectory -- ^ Directory containing the Cardano node configuration files
    -> m PABServerConfig -- ^ Node configuration inside the PAB configuration
getCardanoNodeConfig network nodeDir@(NodeDirectory nd) = do
    -- Read full '<NETWORK>-shelley-genesis.json' config file.
    let shelleyConfigFpath =
             nd
         </> (map toLower $ show network)
          <> "-shelley-genesis.json"
    shelleyGenesisConfigM <- readShelleyGenesis shelleyConfigFpath
    shelleyGenesisConfig <-
        maybe (liftIO $ putStrLn ("Could not parse " <> shelleyConfigFpath) >> exitFailure)
              pure
              shelleyGenesisConfigM

    let networkMagic = sgNetworkMagic shelleyGenesisConfig
    let networkId =
            case network of
                Mainnet -> C.Mainnet
                Testnet -> C.Testnet $ C.NetworkMagic networkMagic

    let securityParam = sgSecurityParam shelleyGenesisConfig

    let slotLength = floor
                   $ (1e3 *)
                   $ nominalDiffTimeToSeconds
                   $ sgSlotLength shelleyGenesisConfig

    let systemStartPosix = floor
                    $ (1e3 *)
                    $ nominalDiffTimeToSeconds
                    $ utcTimeToPOSIXSeconds
                    $ sgSystemStart shelleyGenesisConfig

    pure $ def { pscSocketPath = unNodeSocketPath $ nodeSocketPath nodeDir
               , pscKeptBlocks = fromIntegral securityParam
               , pscSlotConfig = SlotConfig slotLength (POSIXTime systemStartPosix)
               , pscNetworkId = NetworkIdWrapper networkId
               , pscNodeMode = AlonzoNode
               }
  where
    readShelleyGenesis
        :: (MonadIO m) => FilePath -> m (Maybe (ShelleyGenesis StandardShelley))
    readShelleyGenesis = liftIO . Aeson.decodeFileStrict

getNetworkName :: C.NetworkId -> NetworkName
getNetworkName C.Mainnet    = Mainnet
getNetworkName C.Testnet {} = Testnet

