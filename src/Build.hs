#! /usr/bin/env nix-shell
-- #! nix-shell -i "ghci -fdefer-type-errors"
#! nix-shell -i 'runhaskell --ghc-arg=-threaded --ghc-arg=-Wall'
#! nix-shell -p 'ghc.withPackages (p: with p; [ shake pandoc wreq lens bytestring text dhall ])'
#! nix-shell -p unzip coreutils eject imagemagick graphviz
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

main :: IO ()
main = runShakeBuild

runShakeBuild :: IO ()
runShakeBuild = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want [buildDir </> "presentation.html"]

  phony "clean" $ removeFilesAfter "_build" ["//*"]

  buildDir </> revealjsZip %> \out -> download revealjsUrl out

  buildDir </> extractedRevealjs %> \_ -> do
    need [buildDir </> revealjsZip]
    putNormal "Extracting..."
    unzipInBuildDir revealjsZip
    renameRevealJs

  buildDir </> "presentation.html" %> \out -> do
    let inp = takeFileName (out -<.> "md")
    need [inp, buildDir </> extractedRevealjs]
    pandocToReveal inp out

  "*.md" %> \out -> do
    content <- liftIO $ readFile out
    urls <- liftIO . runIO $ listImages <$> readMarkdown def (TS.pack content)
    case urls of
      Left e -> fail (show e)
      Right images -> need ((buildDir </>) <$> images)

  buildDir </> "images/*.jpg" %> \out -> do
    let inp = joinPath . drop 1 . splitDirectories $ out -<.> "src"
    need [inp]
    ImageSrc uri ts <- readImageSrc inp
    download (TS.unpack uri) out
    for_ ts $ unit . applyTransformation out

  buildDir </> "graphviz/*.png" %> \out -> do
    let inp = joinPath . drop 1 . splitDirectories $ out -<.> "dot"
    need [inp]
    graphviz inp out

graphviz :: FilePath -> FilePath -> Action ()
graphviz inp out = cmd bin ["-Tpng", "-o", out, inp]
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
pandocToReveal inp out = unit $ cmd bin ["-t", "revealjs", "-s", inp, "-o", out]
  where bin = "pandoc" :: String

applyTransformation :: String -> Text -> Action ()
applyTransformation out t = cmd bin (words (TS.unpack t) ++ [out, out])
  where bin = "convert" :: String

listImages :: Pandoc -> [FilePath]
listImages = query urls
  where urls (Image _ _ (src, _)) = [src]
        urls _ = []

download :: String -> FilePath -> Action ()
download uri target = do
  putNormal $ "Downloading '" <> uri <> "' and writing to '" <> target <> "'"
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory target)
    r <- Wreq.get uri
    BL.writeFile target (r ^. Wreq.responseBody)
  putNormal $ "Finished downloading '" <> target <> "'"
