---
title: Shake It - So You Don't Have To Make It
author: Markus Hauck
institute: codecentric
---
# Motivation
\section{Motivation}
# Motivation

Common situation:

- lots of files
- goal: create a result (compiled code, image, ...)
- complex relationship between files

# Intro
\section{Intro}
# Make

> Besides building programs, Make can be used to manage any
> project where some files must be **updated automatically** from
> others whenever the others change.
    
- often used to build C(++) programs/libraries
- others: cabal, stack, sbt, maven, gradle
- most are focused on building programs

# Maintaining A Make Build

![Definitely not fun](images/maintain-make.jpg){width=60%}

# We Wrote Our Own

![Even worse: custom build](images/own-build.jpg){width=80%}

# [Shake](http://shakebuild.com/)

> Shake is a **library** for writing build systems.

- written in Haskell (of course)
- no assumptions about build result
- you can build anything!

# Shake vs Make

- what's the deal about `shake`?
- shake is `monadic`
- make is only `applicative`
- and `Monad` is far more powerful than `Applicative`

# Example
\section{Example}
# Dressing Up

![Dress up for winter](graphviz/dressing.png){width=90%}

# Dressing Up

![Run independent things in parallel](graphviz/dressing_parallel.png){height=60% align=center}

# Dressing Up

- rebuild only files that need to be built

```
> ./Build.hs clean && ./Build.hs
> rm coat && ./Build.hs
> rm 'right sock' && ./Build.hs
```

# The Library
\section{The Library}
# The Library

- `Shake` is meant to be used as a library
- use Haskell to describe your rules

\footnotesize
```haskell
main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["now.time"]

  "*.time" %> \out -> do
    Stdout currentTime <- cmd "date"
    writeFileChanged out currentTime
```
\normalsize

# The Library

It's a library! Awesome because:

- `Turtle` for shell scripts in Haskell
- `Dhall` to handle configs
- `Wreq` for arbitrary http calls
- `Pandoc` for conversions of documents
- ...

# Printing Information

Shake providescommands to print stuff with different levels:

- `putLoud`
- `putNormal`
- `putQuiet`

# Running External Commands

External commands can be run via `cmd`:

\footnotesize

```haskell
cmd "git commit -m test"
cmd "git" ["commit", "-m", "test"]
cmd "git" ["commit", "-m", "this is a test"]
```

\normalsize

# Running External Commands

also supports special arguments:

\footnotesize

```
Cwd <path>
AddEnv "NAME" "VALUE"
Shell
Timeout 4.2
WithStdout True
EchoStdout True
FileStdout <file>
```

\normalsize

and more, see `CmdOption` on hackage

# Running External Commands

Example: `unzip` a file

\footnotesize

```haskell
cmd [Cwd "/tmp/test/", EchoStderr True] "unzip" ["-o", "test.zip"]
```

\normalsize

# Running External Commands

Another: run `latexmk`

\footnotesize

```haskell
cmd [Cwd cwd
    ,WithStdout True
    ,EchoStdout False
    ,EchoStderr False
    ,Stdin ""
    ] bin ["-g", "-shell-escape", "-pdf", inp]
```

\normalsize

# Running External Commands

The *output* is also flexible:

```haskell
examples = do
  Stdout stdout <- cmd "date"
  (Exit code, Stderr stderr) <- cmd "date"
  CmdTime t <- cmd "sleep 1"
  Process handle <- cmd "wget haskell.org"
  return ()
```

See [CmdResult](https://hackage.haskell.org/package/shake-0.16/docs/Development-Shake.html#t:CmdResult)

# Working With Files

Shake provides many helpful functions:

\footnotesize

```haskell
copyFile' old new
copyFileChanged old new

readFile' file
writeFileChanged file content

removeFiles dir [pattern1, pattern2]
removeFilesAfter

withTempFile
withTempDir

-- ... many more
```

\normalsize

# How To Write Builds: Rules

1. simple rules from patterns with `%>`
2. match multiple patterns with `|%>`
3. even more power with `?>`: boolean predicate
4. what if a single rule builds multiple files? `&%>`

# Depending on input

Use `need` to depend on input files

\footnotesize
```haskell
need :: [FilePath] -> Action ()
```
\normalsize

- all arguments in the list can be built in parallel
- *easy to forget*

# Show Me The Monads Already

The two important monadic datatypes in Shake:

- Monad to generate rules: `Rules :: * -> *`

\footnotesize

```haskell
newtype Rules a = 
  Rules (WriterT SRules 
          (ReaderT ShakeOptions IO) 
            a)
```
            
\normalsize

- Monad to describe build actions: `Action :: * -> *`

\footnotesize

```haskell
newtype Action a = 
    Action (ReaderT (S Global Local) 
             (ContT () IO) 
               a)
```

\normalsize

- `Action` has an `MonadIO` instance

# Show Me The Monads Already

![Monads](screenshots/2018-01-18-195324_1552x605_scrot.png){width=100%}

# Depending On Non-Files

Shake also supports tracking of other things

1. **contents** of a directory
2. environment variables
3. arbitrary code via oracles

# Custom Caching

# Writing Commands: Phony

# Case Study: Presentations

# Pictures: Manual
- google for picture
- download picture
- resize picture
- include in presentation
- where to store it? git?

# Pictures: Automatic
- reference image in presentatio
- define how to download and how to resize
- use shake to perform all the steps

# How to do it

# But what if I have a lot of images?

- Dowloading hundreds of pics at the same time?
- Problem: it's not CPU bound
- Shake has two ways to limit this

# Resources And Throttling

1. `newResource` limit max concurrent usage
2. `newThrottle` limit usage per time unit

# Source Code

- including code quickly leads to a mess
- write code in slide works
- modifying is a nightmare
- let's shake it

# Source Code

the plan:

- find all snippets in the presentation
- extract them into files
- check them with `hlint`

# Source Code

- lucky: I'm using `pandoc` (amazing!)
- pandoc allows us to parse and modify the AST

\footnotesize

```haskell
extractCodeBlocks :: Pandoc -> [String]
extractCodeBlocks = query codeBlocks
  where codeBlocks (CodeBlock (_,classes,_) content) 
          | "haskell" `elem` classes = [content]
          | otherwise = []
        codeBlocks _ = []
```

\normalsize

# Case Studies
\section{Case Studies}

# Producing the presentation

This whole presentation is built with Shake

- all figures are compiled from sources
- images are downloaded
- latex is compiled
- reveal.js downloaded

# Organizing Photos

- basis: `.RAW` files
- editing creates a "sidecar" file without touching the `.RAW`
- run program to produce `.jpg` from it
- create a smaller version for quick sharing
- pretty cpu intensive! (cannot do all in parallel)
- only do it for changed files

# Getting Information Out Of Shake

- shake can produce reports
- view command table/plot
- view rule table/plot
- display progress during build
- trace haskell IO actions for reports
