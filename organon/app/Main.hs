module Main where

import Data.Yaml qualified as Y
import Site.Organon.Config qualified as Config
import UnliftIO (catch)
import UnliftIO.Directory
import Site.Organon.Server (runOrganon)

main :: IO ()
main = do
  setCurrentDirectory "/home/lucas/dados/projetos/sites/gatil"
  cfg <-
    Config.loadConfigWithDefault "organon.yaml"
      `catch` (error . toText . Y.prettyPrintParseException)
  runOrganon cfg
