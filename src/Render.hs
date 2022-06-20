{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Render where
import Ema
import Heist
import Heist.Interpreted
import Heist.Splices (ifElseISplice)
import Optics.Core
import Org.Exporters.Heist
import Ema.Route.Encoder (RouteEncoder)
import Data.Generics.Product

ifElseSpliceWith :: Monad m => Bool -> Splices (Splice m) -> Splice m
ifElseSpliceWith p splices = localHS (bindSplices splices) $ ifElseISplice p

type HeistS = Maybe (HeistState Exporter)

type HSModel s = HasField' "heistS" s HeistS

heistOutput ::
  (HSModel model) =>
  (r -> RouteEncoder model r -> model -> HeistState Exporter -> Asset LByteString) ->
  RouteEncoder model r -> model -> r -> Asset LByteString
heistOutput f enc m r =
  case view (field' @"heistS") m of
   Just hs -> f r enc m hs
   Nothing -> error "Heist exporter state is empty!"

renderAsset :: Splice Exporter -> HeistState Exporter -> Asset LByteString
renderAsset s hs = AssetGenerated Html $ renderSpliceToDoc hs renderSettings s

renderSettings :: ExporterSettings
renderSettings = defaultExporterSettings
  { headlineLevelShift = 1
  , orgExportHeadlineLevels = 8
  }
