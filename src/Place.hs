-- |

module Place where
import System.FilePath

data Place = Place {relative :: FilePath, directory :: FilePath}

absolute :: Place -> FilePath
absolute p = directory p </> relative p
