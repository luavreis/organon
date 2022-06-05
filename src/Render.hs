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
import Control.Monad.Logger (MonadLogger)
import UnliftIO (MonadUnliftIO)
import Ema.Route.Encoder (RouteEncoder, mapRouteEncoderRoute)
import Data.LVar qualified as LVar
import Data.Generics.Product
import Data.Generics.Wrapped (_Unwrapped)

ifElseSpliceWith :: Monad m => Bool -> Splices (Splice m) -> Splice m
ifElseSpliceWith p splices = localHS (bindSplices splices) $ ifElseISplice p

type HState = Maybe (HeistState Exporter)

type HSModel s = HasField' "hState" s HState

heistAdapter ::
  forall m model.
  (MonadUnliftIO m, MonadLogger m, HSModel model) =>
  LVar.LVar HState ->
  Dynamic m model -> m (Dynamic m model)
heistAdapter hvar d0 = do
  h0 <- LVar.get hvar
  pure $ liftA2 (set (field' @"hState")) (Dynamic (h0, hLoop)) d0
  where
    hLoop :: (HState -> m ()) -> m ()
    hLoop send = do
      lid <- LVar.addListener hvar
      forever do
        send =<< LVar.listenNext hvar lid

heistOutput ::
  (HSModel model) =>
  (r -> RouteEncoder model r -> model -> HeistState Exporter -> Asset LByteString) ->
  RouteEncoder model r -> model -> r -> Asset LByteString
heistOutput f enc m r =
  case view (field' @"hState") m of
   Just hs -> f r enc m hs
   Nothing -> error "Heist exporter state is empty!"

renderAsset :: Splice Exporter -> HeistState Exporter -> Asset LByteString
renderAsset s hs = AssetGenerated Html $ renderSpliceToDoc hs renderSettings s

renderSettings :: ExporterSettings
renderSettings = defaultExporterSettings
  { headlineLevelShift = 1
  , orgExportHeadlineLevels = 8
  }

newtype HeistRoute a = HeistRoute {unHeistRoute :: a}
  deriving (Generic)
  deriving newtype (Eq, Show, IsRoute)

instance (EmaSite r, HSModel (RouteModel r)) => EmaSite (HeistRoute r) where
  type SiteArg (HeistRoute r) = (SiteArg r, LVar.LVar HState)
  siteInput act enc (arg, hvar) = heistAdapter hvar =<< siteInput @r act newEnc arg
    where
      newEnc = mapRouteEncoderRoute _Unwrapped enc
  siteOutput enc m r = siteOutput newEnc m (unHeistRoute r)
    where
      newEnc = mapRouteEncoderRoute _Unwrapped enc
