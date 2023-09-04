{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE RankNTypes #-}

module Site.Org.Render.Types (
  module Site.Org.Render.Types,
  module Org.Exporters.Common,
)
where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Logger (MonadLogger (..), MonadLoggerIO (..))
import Ondim.Targets.HTML (HtmlNode)
import Optics.Core (Prism')
import Org.Exporters.Common hiding (
  Expansion,
  ExpansionMap,
  GlobalExpansion,
  Ondim,
  OndimState,
  SomeExpansion,
 )
import Org.Exporters.Common qualified as EC
import Site.Org.Route (Route)

type RenderM m = (MonadIO m, MonadLoggerIO m)

type Ondim a = EC.Ondim RenderT a

type OndimState = EC.OndimState RenderT

type HtmlBackend = EC.ExportBackend RenderT
type Expansion t = EC.Expansion RenderT t
type GlobalExpansion = EC.GlobalExpansion RenderT
type SomeExpansion = EC.SomeExpansion RenderT
type ExpansionMap = EC.ExpansionMap RenderT

type RPrism = Prism' FilePath Route

newtype RenderT a = RenderT
  { unRenderT ::
      forall m.
      RenderM m => m a
  }

liftRenderT :: RenderT a -> Ondim a
liftRenderT = lift

liftToRenderT :: (forall m. RenderM m => m a) -> RenderT a
liftToRenderT = RenderT

instance Functor RenderT where
  fmap f (RenderT a) = RenderT $ f <$> a

instance Applicative RenderT where
  pure a = RenderT $ pure a
  (RenderT f) <*> (RenderT a) = RenderT $ f <*> a
  liftA2 f (RenderT a) (RenderT b) = RenderT $ liftA2 f a b

instance Monad RenderT where
  (RenderT a) >>= f = RenderT $ a >>= unRenderT . f

instance MonadError [HtmlNode] RenderT where
  throwError = throwError
  catchError (RenderT m) c = catchError m (unRenderT . c)

instance MonadIO RenderT where
  liftIO a = RenderT $ liftIO a

instance MonadLogger RenderT where
  monadLoggerLog x y z w = RenderT $ monadLoggerLog x y z w

instance MonadLoggerIO RenderT where
  askLoggerIO = liftToRenderT askLoggerIO
