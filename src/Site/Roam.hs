-- |

module Site.Roam (RoamRoute) where
import Ema
import Site.Roam.Model
import Site.Roam.Process
import System.UnionMount (mount, FileAction (..))
import Org.Parser (parseOrgIO, defaultOrgOptions)
import Render (HeistSite (..))
import Site.Roam.Render (renderIndex, renderPost)
import System.FilePath ((</>))

instance HeistSite RoamRoute where
  hSiteInput _ _ _ = Dynamic <$> mount' handler
    where
      source = "/home/lucas/dados/notas"
      include = [((), "**/*.org")]
      exclude =
        [ "**/imports/**/*"
        , "**/.*/**/*"
        , "**/.*"
        ]
      mount' = mount source include exclude initialModel . const
      handler fp = \case
        Refresh _ () -> do
          orgdoc <- parseOrgIO defaultOrgOptions (source </> fp)
          let place = Place fp source
          pure $ processRoam orgdoc place
        Delete -> pure $ deleteRD fp
  hSiteOutput enc m = \case
    RoamRoute_Index -> renderIndex enc m
    RoamRoute_Post uid -> renderPost uid enc m
