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
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Ema.Asset (Asset)
import Ondim.Targets.HTML (HtmlNode)
import Optics.Core (Prism')
import Org.Exporters.Common hiding (
  Expansion,
  ExpansionMap,
  Filter,
  GlobalExpansion,
  Ondim,
  OndimState,
  SomeExpansion,
 )
import Org.Exporters.Common qualified as EC
import Org.Exporters.HTML qualified as EC
import Site.Org.Route
import Text.XmlHtml qualified as X

type RenderM m = (MonadIO m, MonadLoggerIO m)

type Ondim a = EC.Ondim RenderT a

type OndimState = EC.OndimState RenderT

type HtmlBackend = EC.HtmlBackend RenderT
type Expansion t = EC.Expansion RenderT t
type GlobalExpansion = EC.GlobalExpansion RenderT
type SomeExpansion = EC.SomeExpansion RenderT
type ExpansionMap = EC.ExpansionMap RenderT
type Filter t = EC.Filter RenderT t

type Layouts = Map Text (X.Document, FilePath)

type RPrism = Prism' FilePath Route

newtype RenderT a = RenderT
  { unRenderT ::
      forall m.
      RenderM m =>
      EitherT [HtmlNode] m a
  }

liftRenderT :: RenderT a -> Ondim a
liftRenderT = lift . lift

liftToRenderT :: (forall m. RenderM m => m a) -> RenderT a
liftToRenderT x = RenderT $ lift x

instance Functor RenderT where
  fmap f (RenderT a) = RenderT $ f <$> a

instance Applicative RenderT where
  pure a = RenderT $ pure a
  (RenderT f) <*> (RenderT a) = RenderT $ f <*> a
  liftA2 f (RenderT a) (RenderT b) = RenderT $ liftA2 f a b

instance Monad RenderT where
  (RenderT a) >>= f = RenderT $ a >>= unRenderT . f

instance MonadError [HtmlNode] RenderT where
  throwError e = RenderT $ throwError e
  catchError (RenderT m) c = RenderT $ catchError m (unRenderT . c)

instance MonadIO RenderT where
  liftIO a = RenderT $ liftIO a

instance MonadLogger RenderT where
  monadLoggerLog x y z w = RenderT $ monadLoggerLog x y z w

instance MonadLoggerIO RenderT where
  askLoggerIO = liftToRenderT askLoggerIO

runRenderT :: RenderT a -> RenderT (Either [HtmlNode] a)
runRenderT (RenderT x) = RenderT $ lift $ runEitherT x

data OndimOutput
  = AssetOutput (Ondim (Asset LByteString))
  | PageOutput Text (X.Document -> FilePath -> Ondim (Asset LByteString))
