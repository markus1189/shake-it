#! /usr/bin/env nix-shell
#! nix-shell -i 'runhaskell --ghc-arg=-threaded --ghc-arg=-Wall'
#! nix-shell -p 'ghc.withPackages (p: with p; [ shake pandoc wreq lens bytestring text ])'
#! nix-shell -p unzip coreutils eject
#! nix-shell --pure
module Main where

import Data.Maybe (listToMaybe)
import Text.Pandoc
import Text.Pandoc.Walk
import Development.Shake
import Development.Shake.FilePath
import Control.Lens.Operators ((^.))
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)

revealjsVersion = "3.5.0"
revealjsUrl = "https://github.com/hakimel/reveal.js/archive/" <> revealjsVersion <> ".zip"
revealjsZip = "revealjs.zip"
extractedRevealjs = "reveal.js/README.md"

buildDir = "_build"

main = runShakeBuild

runShakeBuild = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want [buildDir </> "presentation.html"]

  phony "clean" $ removeFilesAfter "_build" ["//*"]

  buildDir </> revealjsZip %> \out -> download revealjsUrl out

  buildDir </> extractedRevealjs %> \out -> do
    need [buildDir </> revealjsZip]
    putNormal "Extracting..."
    unit $ cmd [Cwd buildDir] "unzip" ["-o", revealjsZip]
    unit $ cmd [Cwd buildDir] "rename" ["reveal.js-" <> revealjsVersion
                                       ,"reveal.js"
                                       ,"reveal.js-" <> revealjsVersion]

  buildDir </> "presentation.html" %> \out -> do
    let inp = takeFileName (out -<.> "md")
    need [inp, buildDir </> extractedRevealjs]
    unit $ cmd "pandoc" ["-t", "revealjs", "-s", inp, "-o", out]

  "*.md" %> \out -> do
    content <- liftIO $ readFile out
    let urls = listImages <$> readMarkdown def content
    case urls of
      Left e -> fail (show e)
      Right images -> need ((buildDir </>) <$> images)

  buildDir </> "images/*.jpg" %> \out -> do
    let inp = joinPath . drop 1 . splitDirectories $ out -<.> "url"
    putNormal inp
    need [inp]
    url <- listToMaybe . lines <$> liftIO (readFile inp)
    case url of
      Nothing -> fail $ "No url found inside '" <> inp <> "'"
      Just u -> download u out

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
