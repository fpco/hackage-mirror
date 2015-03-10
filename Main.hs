import Control.Applicative
import Hackage.Mirror
import Hackage.Mirror.Types
import Options.Applicative

options :: Parser Options
options = Options
    <$> switch (long "verbose" <> help "Display verbose output")
    <*> switch (long "rebuild" <> help "Don't mirror; used for rebuilding")
    <*> strOption (long "from" <> value hackageBaseUrl
                   <> help "Base URL to mirror from")
    <*> strOption (long "to" <> help "Base URL of server mirror to")
    <*> strOption (long "access" <> value "" <> help "S3 access key")
    <*> strOption (long "secret" <> value "" <> help "S3 secret key")

main :: IO ()
main = execParser opts >>= mirrorHackage
  where
    opts = info (helper <*> options)
                (fullDesc
                 <> progDesc "Mirror the necessary parts of Hackage"
                 <> header "hackage-mirror - mirror only the minimum")
