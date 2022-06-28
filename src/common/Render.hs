{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Render where
import Ema
import Heist
import Heist.Interpreted
import Heist.Splices (ifElseISplice)
import Org.Exporters.Heist
import Data.Generics.Product
import Optics.Core

ifElseSpliceWith :: Monad m => Bool -> Splices (Splice m) -> Splice m
ifElseSpliceWith p splices = localHS (bindSplices splices) $ ifElseISplice p

type HeistS = Maybe (HeistState Exporter)

type HSModel s = HasType HeistS s

heistOutput ::
  (HSModel model) =>
  (r -> Prism' FilePath r -> model -> HeistState Exporter -> Asset LByteString) ->
  Prism' FilePath r -> model -> r -> Asset LByteString
heistOutput f pr m r =
  case getTyped m of
   Just hs -> f r pr m hs
   Nothing -> error "Heist exporter state is empty!"

renderAsset :: Splice Exporter -> HeistState Exporter -> Asset LByteString
renderAsset s hs = AssetGenerated Html $ renderSpliceToDoc hs renderSettings s

renderSettings :: ExporterSettings
renderSettings = defaultExporterSettings
  { headlineLevelShift = 1
  , orgExportHeadlineLevels = 8
  }
