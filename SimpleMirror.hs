{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Aws hiding (LogLevel, logger)
import qualified Aws.S3 as Aws
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Applicative
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted hiding (yield)
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Failure
import           Control.Monad hiding (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Morph
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Control.Retry
import qualified Crypto.Hash.SHA512 as SHA512
import           Data.Attempt (isFailure)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Lazy as CL
import qualified Data.Conduit.List as CL
import           Data.Conduit.Zlib as CZ
import           Data.Default (def)
import           Data.Int
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.Monoid
import           Data.Serialize hiding (label)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Lazy.Builder
import           Data.Thyme
import           Network.HTTP.Conduit hiding (Response)
import           Options.Applicative as Opt
import           Prelude hiding (catch)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.IO.Unsafe (unsafePerformIO)
import           System.Locale
import           System.Log.FastLogger hiding (check)
import           Text.Shakespeare.Text

data Options = Options
    { verbose     :: Bool
    , rebuild     :: Bool
    , mirrorFrom  :: String
    , mirrorTo    :: String
    , s3AccessKey :: String
    , s3SecretKey :: String
    }

defaultOptions :: Options
defaultOptions = Options
    { verbose     = False
    , rebuild     = False
    , mirrorFrom  = ""
    , mirrorTo    = ""
    , s3AccessKey = ""
    , s3SecretKey = ""
    }

hackageBaseUrl :: String
hackageBaseUrl = "http://hackage.haskell.org"

options :: Opt.Parser Options
options = Options
    <$> switch (long "verbose" <> help "Display verbose output")
    <*> switch (long "rebuild" <> help "Don't mirror; used for rebuilding")
    <*> strOption (long "from" <> value hackageBaseUrl
                   <> help "Base URL to mirror from")
    <*> strOption (long "to" <> help "Base URL of server mirror to")
    <*> strOption (long "access" <> value "" <> help "S3 access key")
    <*> strOption (long "secret" <> value "" <> help "S3 secret key")

instance MonadActive m => MonadActive (LoggingT m) where
    monadActive = lift monadActive

data Package = Package
    { packageName       :: !String
    , packageVersion    :: !String
    , packageCabal      :: !BL.ByteString
    , packageIdentifier :: !ByteString
    , packageTarEntry   :: !Tar.Entry
    }

packageFullName :: Package -> String
packageFullName Package {..} = packageName <> "-" <> packageVersion

data PathKind = UrlPath | S3Path | FilePath

pathKind :: String -> PathKind
pathKind url
    | "http://" `isPrefixOf` url || "https://" `isPrefixOf` url = UrlPath
    | "s3://" `isPrefixOf` url = S3Path
    | otherwise = FilePath

indexPackages :: (MonadLogger m, MonadThrow m, MonadBaseControl IO m,
                  MonadActive m)
              => Source m ByteString -> Source m Package
indexPackages src = do
    lbs <- lift $ CL.lazyConsume src
    sinkEntries $ Tar.read (BL.fromChunks lbs)
  where
    sinkEntries (Tar.Next ent entries)
        | Tar.NormalFile cabal _ <- Tar.entryContent ent = do
            case splitDirectories (Tar.entryPath ent) of
                [name, vers, _] ->
                    yield $ Package name vers cabal
                        (T.encodeUtf8 (T.pack (name <> vers))) ent
                ["preferred-versions"] -> return ()
                _ -> $(logError) $ "Failed to parse package name: "
                               <> T.pack (Tar.entryPath ent)
            sinkEntries entries
        | otherwise = sinkEntries entries
    sinkEntries Tar.Done = return ()
    sinkEntries (Tar.Fail e) =
        monadThrow $ userError $ "Failed to read tar file: " ++ show e

downloadFromPath :: MonadResource m => String -> String -> Source m ByteString
downloadFromPath path file = do
    let p = path </> file
    exists <- liftIO $ doesFileExist p
    when exists $ CB.sourceFile p

downloadFromUrl :: (MonadResource m, MonadBaseControl IO m,
                    Failure HttpException m)
                => Manager -> String -> String -> Source m ByteString
downloadFromUrl mgr path file = do
    req  <- lift $ parseUrl (path </> file)
    resp <- lift $ http req mgr
    (src, _fin) <- lift $ unwrapResumable (responseBody resp)
    -- jww (2013-11-20): What to do with fin?
    src

withS3 :: MonadResource m
       => Aws.Bucket -> String -> (Aws.Bucket -> String -> m a) -> m a
withS3 url file f = case splitDirectories (T.unpack url) of
    ["s3:", bucket] -> f (T.pack bucket) file
    ["s3:", bucket, prefix] -> f (T.pack bucket) $ prefix </> file
    _ -> monadThrow $ userError $ "Failed to parse S3 path: " ++ T.unpack url

awsRetry :: (MonadIO m, Transaction r a)
         => Configuration
         -> ServiceConfiguration r NormalQuery
         -> Manager
         -> r
         -> ResourceT m (Response (ResponseMetadata a) a)
awsRetry cfg svcfg mgr r =
    transResourceT liftIO $
        retrying def (isFailure . responseResult) $ aws cfg svcfg mgr r

downloadFromS3 :: MonadResource m
               => Configuration
               -> Aws.S3Configuration NormalQuery
               -> Manager
               -> Aws.Bucket
               -> String
               -> Source m ByteString
downloadFromS3 cfg svccfg mgr bucket file = withS3 bucket file go where
    go bucket' (T.pack -> file') = do
        res  <- liftResourceT $
            awsRetry cfg svccfg mgr $ Aws.getObject bucket' file'
        case readResponse res of
            Left (_ :: SomeException) -> return ()
            Right gor -> do
                -- jww (2013-11-20): What to do with fin?
                (src, _fin) <- liftResourceT $ unwrapResumable $
                    responseBody (Aws.gorResponse gor)
                hoist liftResourceT src

download :: (MonadResource m, MonadBaseControl IO m, Failure HttpException m)
         => Configuration
         -> Aws.S3Configuration NormalQuery
         -> Manager
         -> String               -- ^ The server path, like /tmp/foo
         -> String               -- ^ The file's path within the server path
         -> Source m ByteString
download _ _ mgr path@(pathKind -> UrlPath) =
    downloadFromUrl mgr path
download cfg svccfg mgr path@(pathKind -> S3Path) =
    downloadFromS3 cfg svccfg mgr (T.pack path)
download _ _ _ path = downloadFromPath path

uploadToPath :: MonadResource m
             => String -> String -> Source m ByteString -> m ()
uploadToPath path file src = do
    let p = path </> file
    liftIO $ createDirectoryIfMissing True (takeDirectory p)
    src $$ CB.sinkFile p

uploadToUrl :: (MonadResource m, MonadBaseControl IO m, Failure HttpException m)
              => Manager -> String -> String -> Source m ByteString -> m ()
uploadToUrl _mgr _path _file _src = error "uploadToUrl not implemented"

uploadToS3 :: (MonadResource m, m ~ ResourceT IO)
           => Configuration
           -> Aws.S3Configuration NormalQuery
           -> Manager
           -> Aws.Bucket
           -> String
           -> Source m ByteString
           -> m ()
uploadToS3 cfg svccfg mgr bucket file src = withS3 bucket file go where
    go bucket' (T.pack -> file') = do
        lbs <- src $$ CB.sinkLbs
        res <- awsRetry cfg svccfg mgr $
            Aws.putObject bucket' file' (RequestBodyLBS lbs)
        -- Reading the response triggers an exception if one occurred during
        -- the upload.
        void $ readResponseIO res

upload :: (MonadResource m, m ~ ResourceT IO)
       => Configuration
       -> Aws.S3Configuration NormalQuery
       -> Manager
       -> String
       -> String
       -> Source m ByteString
       -> m ()
upload cfg svccfg mgr path@(pathKind -> S3Path) =
    uploadToS3 cfg svccfg mgr (T.pack path)
upload _ _ _ path = uploadToPath path

backingFile :: MonadResource m
           => FilePath
           -> Conduit ByteString m ByteString
backingFile fp =
    bracketP (liftIO (openFile fp WriteMode)) (liftIO . hClose) loop
  where
    loop h = do
        mres <- await
        case mres of
            Nothing -> do
                liftIO $ hClose h
                CB.sourceFile fp
            Just res -> do
                liftIO $ B.hPut h res
                loop h

main :: IO ()
main = execParser opts >>= mirrorHackage
  where
    opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "Mirror the necessary parts of Hackage"
                 <> header "simple-mirror - mirror only the minimum")

mirrorHackage :: Options -> IO ()
mirrorHackage Options {..} =
    runLogger $ withManager $ \mgr -> do
        sums <- getChecksums mgr
        putChecksums mgr "00-checksums.bak" sums
        newSums <- liftIO $ newTVarIO sums
        changed <- liftIO $ newTVarIO False
        void $ go mgr sums newSums changed `finally` do
            ch <- liftIO $ readTVarIO changed
            when ch $ do
                sums' <- liftIO $ readTVarIO newSums
                putChecksums mgr "00-checksums.dat" sums'
  where
    go mgr sums newSums changed = do
        ents <- CL.lazyConsume $
            getEntries mgr $= processEntries mgr sums newSums changed

        -- Use a temp file as a "backing store" to accumulate the new tarball.
        -- Only when it is complete and we've reached the end normally do we
        -- copy the file onto the server.  The checksum file is saved in all
        -- cases so that we know what we mirrored in this session; the index
        -- file, meanwhile, is always valid (albeit temporarily out-of-date if
        -- we abort due to an exception).
        withTemp "index" $ \temp -> do
            CB.sourceLbs (Tar.write ents)
                $= CZ.compress 7 (WindowBits 31) -- gzip compression
                $$ CB.sinkFile temp

            -- Writing the tarball is what causes the changed bit to be
            -- calculated, so we write it first to a temp file and then only
            -- upload it if necessary.
            ch <- liftIO $ readTVarIO changed
            when ch $ void $ do
                _ <- push mgr "00-index.tar.gz" $ CB.sourceFile temp
                $(logInfo) [st|Uploaded 00-index.tar.gz|]

    processEntries mgr sums newSums changed =
        CL.mapMaybeM $ \pkg@(Package {..}) -> do
            let sha = SHA512.hashlazy packageCabal
                et  = Tar.entryTime packageTarEntry
                new = case M.lookup packageIdentifier sums of
                    Nothing -> True
                    Just (et', _sha') -> et /= et' -- || sha /= sha'
            valid <- if new
                     then mirror mgr pkg sha newSums changed
                     else return True
            return $ mfilter (const valid) (Just packageTarEntry)

    mirror mgr pkg sha newSums changed = do
        let fname = packageFullName pkg
            dir   = "package" </> fname
            upath = addExtension dir ".tar.gz"
            dpath = dir </> addExtension fname ".tar.gz"
            cabal = dir </> addExtension (packageName pkg) ".cabal"
        (el, er) <-
            if rebuild
            then return (Right (), Right ())
            else do
                res <- concurrently
                    (push mgr upath $ download cfg svccfg mgr from dpath)
                    (push mgr cabal $ CB.sourceLbs (packageCabal pkg))
                $(logInfo) [st|Mirrored #{fname}|]
                return res
        case (el, er) of
            (Right (), Right ()) -> liftIO $ atomically $ do
                writeTVar changed True
                modifyTVar newSums $
                    M.insert (packageIdentifier pkg)
                        (Tar.entryTime (packageTarEntry pkg), sha)
                return True
            _ -> return False

    push mgr file src = do
        eres <- try $ liftResourceT $ upload cfg svccfg mgr to file src
        case eres of
            Right () -> return ()
            Left e -> do
                let msg = T.pack (show (e :: SomeException))
                unless ("No tarball exists for this package version"
                        `T.isInfixOf` msg) $
                    $(logError) $ "FAILED " <> T.pack file <> ": " <> msg
        return eres

    getChecksums mgr = do
        sums <- download cfg svccfg mgr to "00-checksums.dat" $$ CB.sinkLbs
        $(logInfo) [st|Downloaded checksums.dat from #{to}|]
        return $ if BL.null sums
                 then M.empty
                 else case decodeLazy sums of
                     Left _    -> M.empty
                     Right res -> M.fromList res

    putChecksums mgr file sums = do
        void $ push mgr file $ yield (encode (M.toList sums))
        $(logInfo) [st|Uploaded #{file}|]

    getEntries mgr = do
        $(logInfo) [st|Downloading index.tar.gz from #{from}|]
        indexPackages $
            download cfg svccfg mgr from "00-index.tar.gz" $= CZ.ungzip

    withTemp prefix f = control $ \run ->
        withSystemTempFile prefix $ \temp h -> hClose h >> run (f temp)

    cfg = Configuration Timestamp Credentials
        { accessKeyID     = T.encodeUtf8 (T.pack s3AccessKey)
        , secretAccessKey = T.encodeUtf8 (T.pack s3SecretKey) }
        (defaultLog (if verbose then Aws.Debug else Aws.Error))

    svccfg = defServiceConfig

    runLogger = flip runLoggingT $ logger $
        if verbose then LevelDebug else LevelInfo

    from = mirrorFrom
    to   = mirrorTo

outputMutex :: MVar ()
{-# NOINLINE outputMutex #-}
outputMutex = unsafePerformIO $ newMVar ()

logger :: LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logger maxLvl _loc src lvl logStr = when (lvl >= maxLvl) $ do
    now <- getCurrentTime
    let stamp = formatTime defaultTimeLocale "%b-%d %H:%M:%S" now
    holding outputMutex $
        putStrLn $ stamp ++ " " ++ renderLevel lvl
                ++ " " ++ renderSource src ++ renderLogStr logStr
  where
    holding mutex = withMVar mutex . const

    renderLevel LevelDebug = "[DEBUG]"
    renderLevel LevelInfo  = "[INFO]"
    renderLevel LevelWarn  = "[WARN]"
    renderLevel LevelError = "[ERROR]"
    renderLevel (LevelOther txt) = "[" ++ T.unpack txt ++ "]"

    renderSource :: LogSource -> String
    renderSource txt
        | T.null txt = ""
        | otherwise  = T.unpack txt ++ ": "

    renderLogStr = T.unpack . T.decodeUtf8 . fromLogStr
