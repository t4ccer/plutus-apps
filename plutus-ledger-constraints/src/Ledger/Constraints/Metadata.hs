{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Ledger.Constraints.Metadata (
  OtherFields(..),
  NftMetadataFiles(..),
  NftMetadataToken(..),
  NftMetadata(..),
  TxMetadata(..),
  mkNftMetadata,
 ) where

import Cardano.Prelude (decodeUtf8, encodeUtf8)
import Data.Aeson (FromJSON, ToJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair, Parser, parseFail)
import Data.ByteString.Base16 (decodeLenient, encode)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Plutus.V1.Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName), unCurrencySymbol, unTokenName)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import Prelude (Eq, Maybe (Just, Nothing), Monoid (mempty), Semigroup ((<>)), Show, elem, filter, fmap, fst, mconcat,
                not, pure, show, traverse, ($), (.), (/=), (<$>), (<*>))

import Data.Maybe (fromMaybe)
import Data.OpenApi (NamedSchema (NamedSchema))
import Data.OpenApi.Internal.Schema (nullarySchema)
import Data.OpenApi.Schema qualified as OpenApi
import GHC.Exts (toList)
import GHC.Generics (Generic)

-- | Arbitrary additional fields
newtype OtherFields = OtherFields {getOtherField :: Map Text Aeson.Value}
  deriving newtype (Semigroup, Monoid)
  deriving stock (Show, Eq, Generic)

-- | Include user-defined additional fields in JSON object
addOtherFields :: OtherFields -> [Pair] -> [Pair]
addOtherFields (OtherFields fields) xs = (xs <>) $ Map.elems $ Map.mapWithKey (.=) fields

parseOtherFieldsExcluding :: [Text] -> Aeson.Object -> Parser OtherFields
parseOtherFieldsExcluding excluded v = do
  let keys = filter (not . (`elem` excluded)) $ fmap fst $ toList v
      f k = OtherFields . Map.singleton k <$> v .: k
  mconcat <$> traverse f keys

addFieldWhenJust :: ToJSON v => Text -> Maybe v -> [Pair] -> [Pair]
addFieldWhenJust _ Nothing xs  = xs
addFieldWhenJust k (Just v) xs = (k .= v) : xs

addFieldWhenNotEmpty :: ToJSON v => Text -> [v] -> [Pair] -> [Pair]
addFieldWhenNotEmpty _  [] xs = xs
addFieldWhenNotEmpty k v xs   = (k .= v) : xs

data NftMetadataFiles = NftMetadataFiles
  { nmfName        :: Text
  , nmfMediaType   :: Text
  , nmfSrc         :: Text
  , nmfOtherFields :: OtherFields
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON NftMetadataFiles where
  toJSON NftMetadataFiles {..} = object $
    addOtherFields nmfOtherFields $
    [ "name" .= nmfName
    , "src" .= nmfSrc
    , "mediaType" .= nmfMediaType
    ]

instance FromJSON NftMetadataFiles where
  parseJSON = withObject "NftMetadataFiles" $ \v -> NftMetadataFiles
    <$> v .: "name"
    <*> v .: "src"
    <*> v .: "mediaType"
    <*> parseOtherFieldsExcluding ["name", "src", "mediaType"] v


data NftMetadataToken = NftMetadataToken
  { nmtName        :: Text
  , nmtImage       :: Text
  , nmtMediaType   :: Maybe Text
  , nmtDescription :: Maybe Text
  , nmtFiles       :: [NftMetadataFiles]
  , nmtOtherFields :: OtherFields
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON NftMetadataToken where
  toJSON NftMetadataToken {..} = object $
    addOtherFields nmtOtherFields $
    addFieldWhenNotEmpty "files" nmtFiles $
    addFieldWhenJust "mediaType" nmtMediaType $
    addFieldWhenJust "description" nmtDescription $
    [ "name" .= nmtName
    , "image" .= nmtImage
    ]

instance FromJSON NftMetadataToken where
  parseJSON = withObject "NftMetadataToken" $ \v -> NftMetadataToken
    <$> v .:  "name"
    <*> v .:  "image"
    <*> v .:? "mediaType"
    <*> v .:? "description"
    <*> (fromMaybe [] <$> (v .:?  "files"))
    <*> parseOtherFieldsExcluding ["name", "image", "mediaType", "description", "files"] v


newtype NftMetadata = NftMetadata
  { nfts :: Map CurrencySymbol (Map TokenName NftMetadataToken)
  }
  deriving stock (Show, Eq, Generic)

instance Semigroup NftMetadata where
  m1 <> m2 = NftMetadata { nfts = Map.unionWith Map.union (nfts m1) (nfts m2) }

instance Monoid NftMetadata where
  mempty = NftMetadata mempty

instance ToJSON NftMetadata where
  toJSON NftMetadata {..} =
    object ("version" .= ("1.0" :: Text) : Map.elems (Map.mapWithKey mkJsonPolicy nfts))
    where
      mkJsonPolicy policyId tokens =
        (toHex . unCurrencySymbol $ policyId) .= (object $ Map.elems $ Map.mapWithKey mkJsonToken tokens)
      mkJsonToken assetName token =
        (toText . unTokenName $ assetName) .= token

      toHex :: BuiltinByteString -> Text
      toHex (BuiltinByteString str) = decodeUtf8 (encode str)

      toText :: BuiltinByteString -> Text
      toText (BuiltinByteString str) = decodeUtf8 str

instance FromJSON NftMetadata where
  parseJSON = withObject "NftMetadata" $ \v -> do
    let policies = filter ((/= "version") . fst)$ toList v
        parsePolicy :: (Text, Aeson.Value) -> Parser NftMetadata
        parsePolicy (policyId, (Aeson.Object v')) = do
          let tokens :: [(Text, Aeson.Value)] = toList v'
          mconcat <$> traverse (\(tokenId, val) -> NftMetadata . Map.singleton (toCs policyId) . Map.singleton (toTn tokenId) <$> Aeson.parseJSON val) tokens
        parsePolicy e = parseFail $ "Expected object, got: " <> show e
        toCs :: Text -> CurrencySymbol
        toCs = CurrencySymbol . BuiltinByteString . decodeLenient . encodeUtf8
        toTn :: Text -> TokenName
        toTn = TokenName . BuiltinByteString . encodeUtf8
    mconcat <$> traverse parsePolicy policies

data TxMetadata = TxMetadata
  { txMetadataNft   :: Maybe NftMetadata
  , txMetadataOther :: OtherFields
  }
  deriving stock (Show, Eq, Generic)

instance Semigroup TxMetadata where
  m1 <> m2 = TxMetadata
    { txMetadataNft = txMetadataNft m1 <> txMetadataNft m2
    , txMetadataOther = txMetadataOther m1 <> txMetadataOther m2
    }

instance ToJSON TxMetadata where
  toJSON TxMetadata {..} =
    object $
      addFieldWhenJust "721" txMetadataNft $
      addOtherFields txMetadataOther []

instance FromJSON TxMetadata where
  parseJSON = withObject "TxMetadata" $ \v -> TxMetadata
    <$> v .: "721"
    <*> parseOtherFieldsExcluding ["721"] v

instance OpenApi.ToSchema TxMetadata where
  declareNamedSchema _ = pure (NamedSchema Nothing nullarySchema)

mkNftMetadata ::
  CurrencySymbol ->
  TokenName ->
  Text ->
  Text ->
  NftMetadata
mkNftMetadata cs tn name image =
  NftMetadata
    $ Map.singleton cs
    $ Map.singleton tn
    $ NftMetadataToken name image Nothing Nothing mempty mempty
