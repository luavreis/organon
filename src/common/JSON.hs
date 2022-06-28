-- |

module JSON
 ( ToJSON (..)
 , FromJSON (..)
 , genericToEncoding
 , genericToJSON
 , genericParseJSON
 , customOptions
 ) where
import Data.Aeson

customOptions :: Options
customOptions = defaultOptions
                { fieldLabelModifier = camelTo2 '-'
                , constructorTagModifier = camelTo2 '-'
                , sumEncoding = TaggedObject "kind" "options"
                }
