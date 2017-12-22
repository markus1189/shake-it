#! /usr/bin/env nix-shell
-- #! nix-shell -i ghci
#! nix-shell -i 'runhaskell --ghc-arg=-threaded --ghc-arg=-Wall'
#! nix-shell -p 'ghc.withPackages (p: with p; [ shake pandoc wreq lens bytestring text dhall ])'
#! nix-shell -p unzip coreutils eject imagemagick
#! nix-shell --pure
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Lens.Operators ((^.))
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (for_)
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
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

  buildDir </> extractedRevealjs %> \out -> do
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
    let urls = listImages <$> readMarkdown def content
    case urls of
      Left e -> fail (show e)
      Right images -> need ((buildDir </>) <$> images)

  buildDir </> "images/*.jpg" %> \out -> do
    let inp = joinPath . drop 1 . splitDirectories $ out -<.> "src"
    putNormal inp
    need [inp]
    ImageSrc url transformations <- readImageSrc inp
    download (TS.unpack url) out
    for_ transformations $ unit . applyTransformation out

unzipInBuildDir :: FilePath -> Action ()
unzipInBuildDir fp = cmd [Cwd buildDir] bin ["-o", fp]
  where bin :: String
        bin = "unzip"

renameRevealJs :: Action ()
renameRevealJs = cmd [Cwd buildDir] bin ["reveal.js-" <> revealjsVersion
                                        ,"reveal.js"
                                        ,"reveal.js-" <> revealjsVersion]
  where bin :: String
        bin = "rename"

pandocToReveal :: String -> String -> Action ()
pandocToReveal inp out = unit $ cmd bin ["-t", "revealjs", "-s", inp, "-o", out]
  where bin :: String
        bin = "pandoc"

applyTransformation :: String -> Text -> Action ()
applyTransformation out t = cmd bin (words (TS.unpack t) ++ [out, out])
  where bin :: String
        bin = "convert"

listImages :: Pandoc -> [FilePath]
listImages = query urls
  where urls (Image _ _ (src, _)) = [src]
        urls _ = []

download :: String -> FilePath -> Action ()
download url target = do
  putNormal $ "Downloading '" <> url <> "' and writing to '" <> target <> "'"
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory target)
    r <- Wreq.get url
    BL.writeFile target (r ^. Wreq.responseBody)
  putNormal $ "Finished downloading '" <> target <> "'"
