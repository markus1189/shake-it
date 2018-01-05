#! /usr/bin/env nix-shell
#! nix-shell -i 'runhaskell --ghc-arg=-threaded --ghc-arg=-Wall'
#! nix-shell -p 'ghc.withPackages (p: with p; [ shake ])'
#! nix-shell --pure

import Development.Shake
import Data.Foldable (for_)

main :: IO ()
main = shakeArgs shakeOptions { shakeThreads = 0 } $ do
  want [ "dressed" ]

  phony "clean" $ do
    removeFilesAfter "." [ "left boot"
                         , "right boot"
                         , "coat"
                         , "hat"
                         , "right sock"
                         , "left sock"
                         , "underwear"
                         , "left glove"
                         , "right glove"
                         , "dressed"
                         , "pullover"
                         , "pants"
                         , "boots"
                         ]

  "dressed" %> \out -> do
    need [ "coat", "hat", "left boot", "right boot", "left glove", "right glove" ]
    putNormal "You are ready to go!"
    createFile out

  "coat" %> \out -> do
    need [ "pullover", "pants" ]
    putNormal ("Putting on: " ++ out)
    createFile out

  ["left boot", "right boot"] |%> \out -> do
    let side = head (words out)
    need [side ++ " sock", "pants"]
    putNormal ("Putting on: " ++ out)
    createFile out

  ["left glove", "right glove"] |%> \out -> do
    let side = head (words out)
    need [side ++ " boot", "pants"]
    putNormal ("Putting on: " ++ out)
    createFile out

  "pants" %> \out -> do
    need ["underwear"]
    putNormal ("Putting on: " ++ out)
    createFile out

  simple $ ["pullover", "underwear", "hat"] ++ map (++ " sock") ["right", "left"]

simple :: [String] -> Rules ()
simple names = for_ names $ \name ->
  name %> \out -> do
    putNormal ("Putting on: " ++ out)
    createFile out

createFile :: FilePath -> Action ()
createFile fp = writeFile' fp []
