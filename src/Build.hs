#! /usr/bin/env nix-shell
-- #! nix-shell deps.nix -i "ghci -fdefer-type-errors"
#! nix-shell deps.nix -i 'runhaskell --ghc-arg=-threaded --ghc-arg=-Wall'
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
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
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
  resDisk <- newResource "Disk" 10

  want ([buildDir </> tgt | tgt <- ["presentation.html", "presentation.pdf"]])

  phony "clean" $ removeFilesAfter "_build" ["//*"]

  buildDir </> "presentation.html" %> \out -> do
    let inp = out -<.> "md"
    need [inp, buildDir </> extractedRevealjs]
    pandocToReveal inp out

  buildDir </> "*.md" %> \out -> do
    let inp = dropDirectory1 out
    need [inp]
    copyFileChanged inp out

  "*.md" %> \out -> do
    alwaysRerun
    pandocAST <- traced "parsing markdown" $ do
      content <- readFile out
      result <- runIO $ readMarkdown (def { readerExtensions = pandocExtensions }) (TS.pack content)
      case result of
        Left e -> fail (show e)
        Right ast -> pure ast
    let images = (buildDir </>) <$> listImages pandocAST
        blocks = extractCodeBlocks pandocAST
    snippetFiles <- forP (zip [(1::Int)..] blocks) $ \(i, block) -> do
      let fname = buildDir </> ("code-block-" ++ show i ++ ".hs")
      withResource resDisk 1 $ writeFileChanged fname block
      pure fname
    need (images ++ snippetFiles)

  buildDir </> "*.hs" %> \out -> do
    hlint out

  buildDir </> extractedRevealjs %> \_ -> do
    need [buildDir </> revealjsZip]
    putNormal "Extracting..."
    unzipInBuildDir revealjsZip
    renameRevealJs

  buildDir </> revealjsZip %> \out ->
    download resDownload revealjsUrl out

  buildDir </> "screenshots" </> "*.png" %> \out -> do
    let inp = dropDirectory1 out
    copyFileChanged inp out

  [ buildDir </> "images/*" <.> ext | ext <- [ "jpg", "png", "gif" ] ] |%> \out -> do
    let inp = dropDirectory1 $ out -<.> "src"
    need [inp]
    ImageSrc uri ts <- traced "image-src" (readImageSrc inp)
    download resDownload (TS.unpack uri) out
    for_ ts $ unit . applyTransformation out

  buildDir </> "graphviz/*.png" %> \out -> do
    let inp = dropDirectory1 $ out -<.> "dot"
    need [inp]
    graphviz inp out

  "//*.pdf" %> \out -> do
    let inp = out -<.> "tex"
    files <- getDirectoryFiles "beamer-theme" ["*.sty"]
    for_ files $ \file -> copyFileChanged ("beamer-theme" </> file) (buildDir </> file)
    need [inp]
    latexmk (takeDirectory inp) (".." </> inp)

  "//*.tex" %> \out -> do
    let inp = out -<.> "md"
    need [inp]
    pandocToBeamer inp out

hlint :: FilePath -> Action ()
hlint file = do
  cmd_ [EchoStdout False, WithStdout True] bin [file]
  where bin = "hlint" :: String

latexmk :: FilePath -> FilePath -> Action ()
latexmk cwd inp = do
  cmd [Cwd cwd
      ,WithStdout True
      ,EchoStdout False
      ,EchoStderr False
      ,Stdin ""
      ] bin ["-g", "-shell-escape", "-pdf", inp]
  where bin = "latexmk" :: String

graphviz :: FilePath -> FilePath -> Action ()
graphviz inp out = do
  cmd bin ["-Tpng", "-Gdpi=300","-o", out, inp]
  where bin = "dot" :: String

unzipInBuildDir :: FilePath -> Action ()
unzipInBuildDir fp = cmd [Cwd buildDir
                         ,EchoStdout False
                         ,EchoStderr False
                         ,WithStdout True
                         ] bin ["-o", fp]
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
                 ,"-M", "listings=true"
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

extractCodeBlocks :: Pandoc -> [String]
extractCodeBlocks = query codeBlocks
  where codeBlocks (CodeBlock (_,classes,_) content) | "haskell" `elem` classes = [content]
                                                     | otherwise = []
        codeBlocks _ = []

download :: Resource -> String -> FilePath -> Action ()
download res uri target = withResource res 1 $ traced "download" $ do
  createDirectoryIfMissing True (takeDirectory target)
  r <- Wreq.get uri
  BL.writeFile target (r ^. Wreq.responseBody)
