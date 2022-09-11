{-# LANGUAGE ConstraintKinds #-}

module Render where

import Ema
import Ondim
import Org.Exporters.Common
import Org.Exporters.HTML
import Org.Exporters.Highlighting.EngraveFaces
import Text.XmlHtml qualified as X

type OS = OndimMS (HTag IO)

type Layouts = Map Text X.Document

data OndimOutput
  = OAsset (OS -> IO (Asset LByteString))
  | OPage Text (OS -> X.Document -> IO (Either OndimException LByteString))

backend :: HtmlBackend IO
backend = defHtmlBackend { srcPretty = engraveFacesHtml }

renderSettings :: ExporterSettings
renderSettings =
  defaultExporterSettings
    { headlineLevelShift = 1,
      orgExportHeadlineLevels = 8
    }
