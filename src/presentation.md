---
title: Shake It - So You Don't Have To Make It
---
# Motivation

Common situation:

- lots of files
- goal: create a result (compiled code, image, ...)
- complex relationship between files

# Make

- well known build-automation tool: `make`

> Besides building programs, Make can be used to manage any
> project where some files must be **updated automatically** from
> others whenever the others change.
    
- often used to build C(++) programs/libraries
- others: cabal, stack, sbt, maven, gradle
- most are focused on building programs

# Maintaining A Make Build

![](images/maintain-make.jpg)

# We Wrote Our Own

![](images/own-build.jpg)

# [Shake](http://shakebuild.com/)

> Shake is a **library** for writing build systems.

- written in Haskell (of course)
- no assumptions about build result
- you can build anything!

# Dressing Up

![](graphviz/dressing.png)

# Simple things first

- independent things are run in parallel

![](graphviz/dressing_parallel.png)

# Simple things first

- incremental rebuild if only one file changes

```
> ./Build.hs clean && ./Build.hs
> rm coat && ./Build.hs
> rm 'right sock' && ./Build.hs
```

# Simple things first

- you can also get a nice report

# But Wait!

There's more!

# Show Me The Monads Already

The two important monadic datatypes in Shake:

1. `Rules :: * -> *`

```scala
newtype Rules a = Rules (WriterT SRules (ReaderT ShakeOptions IO) a)
```

2. `Action :: * -> *`

```scala
newtype Action a = Action (ReaderT (S Global Local) (ContT () IO) a)
```

# Printing Information

Shake provides several commands to print stuff:

1. `putLoud`
2. `putNormal`
3. `putQuiet`

# Running External Commands

# Working With Files

# How To Write Builds: Rules

1. simple rules from patterns with `%>`
2. match multiple patterns with `|%>`
3. even more power with `?>`: boolean predicate
4. what if a single rule builds multiple files? `&%>`

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

# Producing the presentation

# Use Case: Organizing Photos

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
