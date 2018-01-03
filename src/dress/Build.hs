#! /usr/bin/env nix-shell
#! nix-shell -i 'runhaskell --ghc-arg=-threaded --ghc-arg=-Wall'
#! nix-shell -p 'ghc.withPackages (p: with p; [ shake ])'
#! nix-shell --pure

import Development.Shake
import Data.Foldable (for_)

main :: IO ()
main = shakeArgs shakeOptions { shakeThreads = 0 } $ do
  want [ "dressed" ]

  "dressed" %> \out -> do
    need [ "coat", "hat", "left boot", "right boot", "gloves" ]
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

  "gloves" %> \out -> do
    need ["left boot", "right boot", "pants", "pullover"]
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
