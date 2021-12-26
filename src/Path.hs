-- |

module Path where
import Ema
import System.FilePath

type Path = [Slug]

pathToUrl :: Path -> FilePath
pathToUrl = toString . foldr (\s u -> u <> "/" <> encodeSlug s) ""

urlToPath :: FilePath  -> Path
urlToPath = map (decodeSlug . toText) . splitDirectories
