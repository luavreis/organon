{-# LANGUAGE ConstraintKinds #-}

module Render where

import Data.Generics.Product
import Ema
import Ondim
import Ondim.HTML (HtmlNode)
import Optics.Core
import Org.Exporters.Common
import Org.Exporters.HTML
import Relude.Extra.Map ((!?))
import Text.XmlHtml qualified as X

type OS = OndimS HTag HtmlNode

type Layouts = Map Text X.Document

type OndimModel s =
  ( HasType OS s,
    HasType Layouts s
  )

data OndimOutput
  = OAsset (OS -> Asset LByteString)
  | OPage Text (OS -> X.Document -> Either OndimException LByteString)

renderWithLayout :: OndimModel m => m -> OndimOutput -> Asset LByteString
renderWithLayout m =
  let layouts :: Map Text X.Document = m ^. typed
      ostate :: OS = m ^. typed
   in \case
        OAsset x -> x ostate
        OPage lname doc
          | Just layout <- layouts !? lname ->
              AssetGenerated Html $
                either (error . show) id $
                  doc ostate layout
          | otherwise -> error $ "Could not find layout " <> lname

renderSettings :: ExporterSettings
renderSettings =
  defaultExporterSettings
    { headlineLevelShift = 1,
      orgExportHeadlineLevels = 8
    }
