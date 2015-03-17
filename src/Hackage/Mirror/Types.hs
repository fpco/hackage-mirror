module Hackage.Mirror.Types where

import qualified Codec.Archive.Tar as Tar (Entry)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)

data Options = Options
    { verbose     :: Bool
    , rebuild     :: Bool
    , mirrorFrom  :: String
    , mirrorTo    :: String
    , s3AccessKey :: String
    , s3SecretKey :: String
    }

data Package = Package
    { packageName       :: !String
    , packageVersion    :: !String
    , packageCabal      :: !BL.ByteString
    , packageIdentifier :: !ByteString
    , packageTarEntry   :: !Tar.Entry
    }

data PathKind = UrlPath | S3Path | FilePath
