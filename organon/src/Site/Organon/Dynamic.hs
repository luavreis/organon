{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
module Site.Organon.Dynamic (
  layoutDynamic,
  ondimDynamic,
  cacheDynamic,
)
where

import Control.Monad.Logger (MonadLogger, logErrorNS)
import Data.Map (singleton)
import Ema.Dynamic (Dynamic (..))
import Ondim.Targets.HTML.Load (loadTemplatesDynamic)
import Org.Exporters.HTML (htmlTemplateDir)
import Site.Org.Render.Types
import Site.Organon.Cache
import System.FilePath (takeBaseName, (</>))
import System.UnionMount qualified as UM
import Text.XmlHtml qualified as X
import UnliftIO (MonadUnliftIO)

layoutDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m Layouts)
layoutDynamic dir = do
  Dynamic
    <$> UM.mount dir [((), "**/*.html")] [] mempty \() fp _fa ->
      X.parseHTML fp <$> readFileBS (dir </> fp) >>= \case
        Left e -> logErrorNS "Template Loading" (toText e) >> liftIO (fail e)
        Right tpl -> do
          let name = fromString $ takeBaseName fp
          pure (singleton name tpl <>)

ondimDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m OndimMS)
ondimDynamic dir = do
  ddir <- liftIO htmlTemplateDir
  Dynamic <$> loadTemplatesDynamic [dir, ddir]
