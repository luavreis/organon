-- |

module Site.Org.Links where
import Org.Types
import Org.Walk
import System.FilePath (takeExtension, (</>), normalise)
import Control.Monad.Trans.Writer

processLink :: FilePath -> OrgObject -> Writer (Set FilePath) OrgObject
processLink dir (Link (URILink "file" (toString -> path)) contents) = do
    when (takeExtension path `notElem` [".org", ".html"]) $ -- TODO
      tell (one $ normalise $ dir </> path)
    pure $ Link (URILink "file" (toText $ dir </> path)) contents
processLink dir (Image (URILink "file" (toString -> path))) = do
    when (takeExtension path `notElem` [".org", ".html"]) $ -- TODO
      tell (one $ normalise $ dir </> path)
    pure $ Image (URILink "file" (toText $ dir </> path))
processLink _ x = pure x

processLinks :: FilePath -> OrgDocument -> (OrgDocument, Set FilePath)
processLinks dir = runWriter . walkM (processLink dir)
