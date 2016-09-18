#!/usr/bin/env stack
-- stack --resolver lts-6.16 runghc --package http-conduit

{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple
import System.Environment (getArgs)
import System.Process (rawSystem)
import Control.Concurrent (threadDelay)
import System.Exit
import Control.Monad (when)

main :: IO ()
main = do
    args' <- getArgs
    (url, update) <-
        case args' of
            [] -> error errMsg
            url:cmd:args -> return (url, do
                ec <- rawSystem cmd args
                when (ec /= ExitSuccess) (exitWith ec))
    req <- setRequestMethod "HEAD" <$> parseRequest url

    let loop mlastTag = do
            res <- httpLBS req
            let mnewTag = lookup "ETag" $ getResponseHeaders res

            case (mlastTag, mnewTag) of
                (Just last, Just new)
                    | last == new -> putStrLn "No change in index, sleeping"
                _ -> update

            threadDelay $ 1000 * 1000 * 60 -- sleep for a minute

            loop mnewTag

    loop Nothing
  where
    errMsg = "Provide a URL, command and list of arguments to run on index change"
