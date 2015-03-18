{-|
Module      : Main
Description : Create your own a mirror of Hackage (CLI)
Copyright   : (c) FPComplete.com, 2015
License     : MIT
Maintainer  : John Wiegley <johnw@fpcomplete.com>
Stability   : experimental
Portability : POSIX

This module is command line interface to the Hackage.Mirror library.
-}

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Hackage.Mirror ( Options(..), mirrorHackage )
import Options.Applicative
    ( helper,
      execParser,
      value,
      switch,
      strOption,
      progDesc,
      long,
      info,
      help,
      header,
      fullDesc,
      (<>) )

main :: IO ()
main = execParser optsParser >>= mirrorHackage
  where optsParser =
          info (helper <*> options)
               (fullDesc <>
                progDesc "Mirror the necessary parts of Hackage" <>
                header "hackage-mirror - mirror only the minimum")
        options =
          Options <$>
          switch (long "verbose" <>
                  help "Display verbose output") <*>
          switch (long "rebuild" <>
                  help "Don't mirror; used for rebuilding") <*>
          strOption (long "from" <>
                     value "http://hackage.haskell.org" <>
                     help "Base URL to mirror from") <*>
          strOption (long "to" <>
                     help "Base URL of server mirror to") <*>
          strOption (long "access" <>
                     value "" <>
                     help "S3 access key") <*>
          strOption (long "secret" <>
                     value "" <>
                     help "S3 secret key")
