#! /usr/bin/env nix-shell
-- #! nix-shell -i "ghci -fdefer-type-errors"
#! nix-shell -i 'runhaskell --ghc-arg=-threaded --ghc-arg=-Wall'
#! nix-shell -p 'ghc.withPackages (p: with p; [ shake pandoc wreq lens bytestring text dhall ])'
#! nix-shell -p unzip coreutils eject imagemagick graphviz
#! nix-shell -p 'texlive.combine {inherit (texlive) scheme-medium beamer listings minted cleveref microtype babel todonotes chngcntr excludeonly upquote ifplatform xstring enumitem;}'
#! nix-shell --pure
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Lens.Operators ((^.))
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (for_)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import           Development.Shake
import           Development.Shake.FilePath
import qualified Dhall as D
import qualified Network.Wreq as Wreq
import           System.Directory (createDirectoryIfMissing)
import           Text.Pandoc
import           Text.Pandoc.Walk

data ImageSrc = ImageSrc { url :: Text, transformations :: [Text] } deriving (Show, D.Generic)

instance D.Interpret ImageSrc

revealjsVersion :: String
revealjsVersion = "3.5.0"

revealjsUrl :: String
revealjsUrl = "https://github.com/hakimel/reveal.js/archive/" <> revealjsVersion <> ".zip"

revealjsZip :: String
revealjsZip = "revealjs.zip"

extractedRevealjs :: String
extractedRevealjs = "reveal.js/README.md"

buildDir :: FilePath
buildDir = "_build"

readImageSrc :: MonadIO io => String -> io ImageSrc
readImageSrc p = liftIO $ D.input D.auto ("./" <> T.pack p)

myShakeOptions :: ShakeOptions
myShakeOptions = shakeOptions { shakeLint = Just LintBasic
                              , shakeReport = ["report.html", "report.json"]
                              , shakeThreads = 0
                              , shakeColor = True
                              }

main :: IO ()
main = runShakeBuild

runShakeBuild :: IO ()
runShakeBuild = shakeArgs myShakeOptions $ do
  resDownload <- newResource "Download" 10

  want ([buildDir </> tgt | tgt <- ["presentation.html", "presentation.pdf"]])

  phony "clean" $ removeFilesAfter "_build" ["//*"]

  phony "copy-beamer-theme" $ do
    files <- getDirectoryFiles "beamer-theme" ["*.sty"]
    for_ files $ \file -> copyFile' ("beamer-theme" </> file) (buildDir </> file)

  buildDir </> "presentation.html" %> \out -> do
    let inp = takeFileName (out -<.> "md")
    need [inp, buildDir </> extractedRevealjs]
    pandocToReveal inp out

  buildDir </> "*.md" %> \out -> do
    alwaysRerun
    copyFile' (dropDirectories 1 out) out

  "*.md" %> \out -> do
    alwaysRerun
    urls <- traced "parsing markdown" $ do
      content <- readFile out
      runIO $ listImages <$> readMarkdown def (TS.pack content)
    case urls of
      Left e -> fail (show e)
      Right images -> need ((buildDir </>) <$> images)

  buildDir </> extractedRevealjs %> \_ -> do
    need [buildDir </> revealjsZip]
    putNormal "Extracting..."
    unzipInBuildDir revealjsZip
    renameRevealJs

  buildDir </> revealjsZip %> \out -> download resDownload revealjsUrl out

  [ buildDir </> "images/*" <.> ext | ext <- [ "jpg", "png", "gif" ] ] |%> \out -> do
    let inp = dropDirectories 1 $ out -<.> "src"
    need [inp]
    ImageSrc uri ts <- traced "image-src" (readImageSrc inp)
    download resDownload (TS.unpack uri) out
    for_ ts $ unit . applyTransformation out

  buildDir </> "graphviz/*.png" %> \out -> do
    let inp = dropDirectories 1 $ out -<.> "dot"
    need [inp]
    graphviz inp out

  "//*.pdf" %> \out -> do
    let inp = out -<.> "tex"
    need [inp, "copy-beamer-theme"]
    latexmk (takeDirectory inp) (".." </> inp)

  "//*.tex" %> \out -> do
    let inp = out -<.> "md"
    need [inp]
    pandocToBeamer inp out

latexmk :: FilePath -> FilePath -> Action ()
latexmk cwd inp = do
  cmd [Cwd cwd] bin ["-silent", "-g", "-shell-escape", "-pdf", inp]
  where bin = "latexmk" :: String

graphviz :: FilePath -> FilePath -> Action ()
graphviz inp out = do
  cmd bin ["-Tpng", "-Gdpi=300","-o", out, inp]
  where bin = "dot" :: String

unzipInBuildDir :: FilePath -> Action ()
unzipInBuildDir fp = cmd [Cwd buildDir] bin ["-o", fp]
  where bin = "unzip" :: String

renameRevealJs :: Action ()
renameRevealJs = cmd [Cwd buildDir] bin ["reveal.js-" <> revealjsVersion
                                        ,"reveal.js"
                                        ,"reveal.js-" <> revealjsVersion]
  where bin = "rename" :: String

pandocToReveal :: String -> String -> Action ()
pandocToReveal inp out = do
  needed [inp]
  unit $ cmd bin ["-V", "theme=solarized"
                 ,"-t", "revealjs"
                 ,"-s", inp
                 ,"-o", out
                 ]
  where bin = "pandoc" :: String

pandocToBeamer :: String -> String -> Action ()
pandocToBeamer inp out = do
  needed [inp]
  unit $ cmd bin ["-V", "theme=codecentric"
                 ,"-V", "navigation=horizontal"
                 ,"-V", "titlegraphic="
                 ,"-t", "beamer"
                 ,"--listings"
                 ,"--highlight-style", "haddock"
                 ,"-s", inp
                 ,"-o", out
                 ]
  where bin = "pandoc" :: String

applyTransformation :: String -> Text -> Action ()
applyTransformation out t = cmd bin (words (TS.unpack t) ++ [out, out])
  where bin = "convert" :: String

listImages :: Pandoc -> [FilePath]
listImages = query urls
  where urls (Image _ _ (src, _)) = [src]
        urls _ = []

download :: Resource -> String -> FilePath -> Action ()
download res uri target = withResource res 1 $ traced "download" $ do
  createDirectoryIfMissing True (takeDirectory target)
  r <- Wreq.get uri
  BL.writeFile target (r ^. Wreq.responseBody)

dropDirectories :: Int -> FilePath -> FilePath
dropDirectories n = joinPath . drop n . splitDirectories
