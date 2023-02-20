{-# LANGUAGE QuasiQuotes #-}

module Site.Organon.Extra.LaTeX.Render where

import Control.Monad.Logger
import Data.Text qualified as T
import Data.Text.IO (hPutStr)
import NeatInterpolation
import Site.Org.Render.Types (RenderT)
import Site.Organon.Extra.LaTeX.Types
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (-<.>))
import System.IO (hClose, openTempFile)
import UnliftIO.Exception (IOException, catch)
import UnliftIO.Process
import UnliftIO.Temporary (withSystemTempDirectory)

renderLaTeX ::
  -- | Document's file path.
  FilePath ->
  -- | Process spec
  LaTeXProcessSpec ->
  -- | Contents
  Text ->
  RenderT (Text, ByteString)
renderLaTeX path process txt = do
  logger <- askLoggerIO
  liftIO $ (`runLoggingT` logger) do
    pipeline `catch` \(_ :: IOException) -> do
      logErrorNS "LaTeX Rendering" ("Failed to render LaTeX in file " <> toText path)
      logErrorNS "LaTeX Rendering" (T.strip $ "LaTeX content:\n" <> finalText)
      pure (process.imageMIMEType, "")
  where
    pipeline :: LoggingT IO (Text, ByteString)
    pipeline =
      withSystemTempDirectory "tex-conversion" $ \tmpDir -> do
        (texInFp, texInH) <- liftIO $ openTempFile tmpDir "orgtex.tex"
        liftIO $ hPutStr texInH finalText
        liftIO $ hClose texInH
        -- Here I'm reimplementing Org's `org-compile-file`, which mandates that the
        -- process produces exactly the same filepath but with replaced extension.
        let texOutFp = texInFp -<.> process.imageInputType
        forM_ process.latexCompiler \cmd ->
          callCreateProcess
            ( shell . toString $
                cmd
                  & T.replace "%o" (toText tmpDir)
                  & T.replace "%O" (toText texOutFp)
                  & T.replace "%f" (toText texInFp)
            )
              { cwd = Just dir
              }
        let iOutFp = texOutFp -<.> process.imageOutputType
        forM_ process.imageConverter \cmd ->
          callCreateProcess
            ( shell . toString $
                cmd
                  & T.replace "%o" (toText tmpDir)
                  & T.replace "%O" (toText iOutFp)
                  & T.replace "%f" (toText texOutFp)
                  & T.replace "%S" (show process.imageSizeAdjust)
            )
              { cwd = Just dir
              }
        (process.imageMIMEType,) <$> readFileBS iOutFp

    finalText =
      let pPreamble = process.preamble
       in [text|
          $pPreamble
          \begin{document}
          $txt
          \end{document}
        |]
    dir = takeDirectory path
    callCreateProcess :: CreateProcess -> LoggingT IO ()
    callCreateProcess p = do
      (ecode, out, err) <- readCreateProcessWithExitCode p ""
      case ecode of
        ExitSuccess -> pure ()
        ExitFailure _ -> do
          logErrorNS "LaTeX Rendering" ("Error in process " <> show (cmdspec p))
          logErrorNS "LaTeX Rendering" (T.strip $ "STDOUT:\n" <> toText out)
          logErrorNS "LaTeX Rendering" (T.strip $ "STDERR:\n" <> toText err)
