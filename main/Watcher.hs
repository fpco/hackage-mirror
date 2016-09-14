#!/usr/bin/env stack
-- stack --resolver lts-6.16 runghc --package http-conduit

{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple
import System.Environment (getArgs)
import System.Process (rawSystem)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    args' <- getArgs
    update <-
        case args' of
            [] -> error errMsg
            url:cmd:args -> return $ rawSystem cmd args >>= print
    req' <- parseUrlThrow url
    let req = req' { method = "HEAD" }

        loop mlastTag count = do
            res <- httpLBS req
            let mnewTag = lookup "ETag" $ getResponseHeaders res

                -- We don't fully trust Hackage to report things
                -- accurately, see:
                -- https://github.com/haskell/hackage-server/issues/537. So
                -- every 10 runs, we force the update action to be run
                -- regardless of whether the ETag changed or not.
                newCount = (count + 1) `mod` 10
            case (mlastTag, mnewTag) of
                (Just last, Just new)
                    | last == new && newCount /= 0
                        -> putStrLn "No change in index, sleeping"
                _ -> update

            threadDelay $ 1000 * 1000 * 60 -- sleep for a minute

            loop mnewTag newCount

    loop Nothing 0
  where
    errMsg = "Provide a URL, command and list of arguments to run on index change"
