module Site.Org.Render
  ( module Site.Org.Render,
    module Org.Exporters.Common,
  )
where

import Control.Monad.Trans.Cont
import Data.IxSet.Typed qualified as Ix
import Data.List qualified as L
import Data.Map.Syntax ((##))
import Data.Text qualified as T
import Data.Time
import Ema
import Ondim.Extra
import Ondim.HTML (HtmlNode (..), HtmlTag)
import Optics.Core
import Org.Exporters.Common hiding (Expansion, Ondim, OndimMS)
import Org.Exporters.Common qualified as EC
import Org.Exporters.HTML (HtmlBackend, defHtmlBackend, render)
import Org.Exporters.Highlighting.EngraveFaces (engraveFacesHtml)
import Org.Exporters.Processing.OrgData (ExporterSettings (..), OrgData (exporterSettings), defaultExporterSettings)
import Org.Types
import Org.Walk (walk)
import Relude.Unsafe (fromJust)
import Site.Org.Model
import Text.XmlHtml qualified as X

type M = ContT (Asset LByteString) IO

type OS = EC.OndimMS HtmlTag M

type Ondim a = EC.Ondim HtmlTag M a

type Expansion t = EC.Expansion HtmlTag M t

type Layouts = Map Text X.Document

data OndimOutput
  = OAsset (OS -> M (Asset LByteString))
  | OPage Text (OS -> X.Document -> M (Either OndimException LByteString))

backend :: HtmlBackend M
backend = defHtmlBackend {srcPretty = engraveFacesHtml}

resolveLink :: Pages -> (Route -> Text) -> OrgObject -> OrgObject
resolveLink m route = \case
  (Link t c) -> Link (resolveTarget t) c
  (Image t) -> Image (resolveTarget t)
  x -> x
  where
    resolveTarget = \case
      (URILink "id" id_)
        | Just page <- Ix.getOne (m Ix.@= OrgID id_) ->
            UnresolvedLink $ route (Route_Page $ _identifier page)
      (URILink uri (toString -> path))
        | Just source <- T.stripPrefix "source:" uri ->
            let ix_ = OrgPath source path
                orgRoute = do
                  ident <- _identifier <$> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= ix_)
                  pure $ Route_Page ident
                staticRoute = do
                  let sroute = StaticFileIx ix_
                  guard $ not $ Ix.null (m Ix.@= sroute)
                  pure $ Route_Static sroute
                finalRoute = fromMaybe (error "TODO") (orgRoute <|> staticRoute)
             in UnresolvedLink $ route finalRoute
      x -> x

bindPage ::
  Text ->
  Prism' FilePath Route ->
  Pages ->
  OrgEntry ->
  Ondim a ->
  Ondim a
bindPage pfx rp mPages page' node =
  bindDocument backend pfx (_document page) node
    `binding` prefixed pfx do
      "tags" ## \inner ->
        join <$> forM (_tags page) \tag ->
          children @HtmlNode inner
            `bindingText` do
              "tag" ## pure tag
      -- TODO: after Ondim is upated, those two will be textual expansions.
      forM_ (_ctime page) \t -> "ctime" ## timeExp t
      forM_ (nonEmpty $ _mtime page) \_ ->
        "mtimes" ## \inner ->
          join <$> forM (_mtime page) \t ->
            children inner
              `binding` do "mtimes:time" ## timeExp t
    `bindingText` prefixed pfx do
      "route" ## pure $ router $ Route_Page $ _identifier page
  where
    page = page' & #_document %~ walk resolve
    resolve = resolveLink mPages router
    router = routeUrl rp

renderPost :: Identifier -> Prism' FilePath Route -> Model -> OndimOutput
renderPost identifier rp m =
  OPage (_layout page) \st ly ->
    render (_orgData page) st $
      bindPage "page:" rp (_mPages m) page $
        liftSubstructures ly
  where
    page = fromJust $ Ix.getOne (_mPages m Ix.@= identifier)

timeExp :: UTCTime -> Expansion HtmlNode
timeExp time node = do
  attrs <- attributes node
  let defFmt = dateTimeFmt defaultTimeLocale
      fmt = maybe defFmt toString (L.lookup "fmt" attrs)
  pure $ one $ TextNode $ toText $ formatTime defaultTimeLocale fmt time

takeExp :: Expansion HtmlNode
takeExp node = do
  attrs <- attributes node
  let n :: Maybe Int =
        readMaybe . toString =<< L.lookup "n" attrs
  maybe id take n <$> children node

unwrapExp :: Expansion HtmlNode
unwrapExp node = do
  child <- children node
  pure $
    foldMap
      ( \case
          Element _ _ _ els -> els
          n@TextNode {} -> [n]
      )
      child
