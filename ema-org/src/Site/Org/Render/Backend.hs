{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Site.Org.Render.Backend (backend) where

import Data.Aeson qualified as Aeson
import Data.IxSet.Typed qualified as Ix
import Data.List qualified as L
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Ema.Route.Url (routeUrl)
import Ondim.Extra.BindJSON (openObject)
import Ondim.Targets.HTML (HtmlNode (TextNode), fromNodeList)
import Optics.Core ((%~))
import Org.Exporters.HTML (HtmlBackend, defHtmlBackend)
import Org.Types (LinkTarget (..), OrgElement (..), OrgObject (..), srcLinesToText)
import Relude.Unsafe (fromJust)
import Site.Org.Model
import Site.Org.Render.Types
import System.FilePath (dropExtension, isExtensionOf)
import Text.XmlHtml qualified as X

backend :: Pages -> RPrism -> HtmlBackend RenderT
backend p rp = fix \self ->
  let def = defHtmlBackend
      m "o" [e] = callExpansion e (nullObj def)
      m _ _ = pure []
   in def
        { macro = m
        , customElement = customElementPipeline p rp self
        , customObject = customObjectPipeline p rp self
        }

customObjectPipeline ::
  Pages -> RPrism -> HtmlBackend RenderT -> OrgObject -> Maybe (Ondim [HtmlNode])
customObjectPipeline m rp bk x =
  asum $
    flap
      [ customLink m rp bk
      ]
      x

customElementPipeline ::
  Pages -> RPrism -> HtmlBackend RenderT -> OrgElement -> Maybe (Ondim [HtmlNode])
customElementPipeline m rp bk x =
  asum $
    flap
      [ customSourceBlock bk
      , customFigure m rp bk
      ]
      x

customSourceBlock ::
  HtmlBackend RenderT -> OrgElement -> Maybe (Ondim [HtmlNode])
customSourceBlock bk = \case
  SrcBlock {..} -> do
    e <- L.lookup "expand" srcBlkArguments
    let content = encodeUtf8 $ srcLinesToText srcBlkLines
    if e == "t" && srcBlkLang == "html"
      then do
        parsed <- rightToMaybe $ X.parseHTML "" content
        return $ liftNodes $ fromNodeList $ X.docContent parsed
      else do
        contentObj :: Aeson.Object <-
          case srcBlkLang of
            "json" -> Aeson.decodeStrict content
            "yaml" -> either (error . show) id $ Yaml.decodeThrow content
            _ -> Nothing
        return $
          openObject @HtmlNode Nothing contentObj $
            bindKeywords bk "akw:" affKws $
              callExpansion e $
                TextNode ""
  _ -> Nothing

customLink :: Pages -> RPrism -> HtmlBackend RenderT -> OrgObject -> Maybe (Ondim [HtmlNode])
customLink m rp bk = \case
  Link tgt descr ->
    expandOrgObject bk <$> (Link <$> resolveTarget m rp tgt ?? descr)
  _ -> Nothing

customFigure :: Pages -> RPrism -> HtmlBackend RenderT -> OrgElement -> Maybe (Ondim [HtmlNode])
customFigure m rp bk = \case
  Paragraph aff [Link tgt []] ->
    expandOrgElement bk <$> (Paragraph aff . one <$> (Link <$> resolveTarget m rp tgt ?? []))
  _ -> Nothing

resolveTarget :: Pages -> RPrism -> LinkTarget -> Maybe LinkTarget
resolveTarget m rp = \case
  (URILink "id" id')
    | (id_, anchor) <- breakInternalRef id'
    , Just page <- Ix.getOne (m Ix.@= OrgID id_) ->
        Just . UnresolvedLink $
          route (Route_Page page.identifier) <> anchor
  (URILink uri (maybeAddHash -> anchor))
    | Just rawOPath <- T.stripPrefix "source:" uri ->
        Just . UnresolvedLink $
          let ix_ :: OrgPath = removeOrgExt $ fromJust $ readMaybe (toString rawOPath)
              orgRoute = do
                ident <- (.identifier) <$> Ix.getOne (m Ix.@= LevelIx 0 Ix.@= ix_)
                pure $ Route_Page ident
              staticRoute = do
                let sroute = StaticFileIx ix_
                guard $ not $ Ix.null (m Ix.@= sroute)
                pure $ Route_Static sroute
           in case orgRoute <|> staticRoute of
                Just finalRoute -> route finalRoute <> anchor
                Nothing -> do
                  error $
                    "This should not happen at this point. An OrgPath link points the "
                      <> prettyOrgPath ix_
                      <> " but it does not exist in the model. Please report it as a bug."
  _ -> Nothing
  where
    removeOrgExt = #relpath %~ \p -> if "org" `isExtensionOf` p then dropExtension p else p
    maybeAddHash x = maybe x ("#" <>) (T.stripPrefix "::" x)
    breakInternalRef = second maybeAddHash . T.breakOn "::"
    route = routeUrl rp
