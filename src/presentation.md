# Shake It - So You Don't Have To Make It

## Motivation

Common situation:
- lots of files
- goal: create a result (compiled code, image, ...)
- complex relationship between files

## Make

- well known build-automation took: `make`

> Besides building programs, Make can be used to manage any
> project where some files must be **updated automatically** from
> others whenever the others change.
    
- often used to build C(++) programs/libraries
- others: cabal, stack, sbt, maven, gradle
- most are focused on building programs

## Maintaining A Make Build

![](images/maintain-make.jpg)

## We Wrote Our Own

![](images/own-build.jpg)

## [Shake](http://shakebuild.com/)

> Shake is a **library** for writing build systems.

- written in Haskell (of course)
- no assumptions about build result
- you can build anything!

## Dressing Up For Winter

![](graphviz/dressing.png)

## Pictures
### Manual

- google for picture
- download picture
- resize picture
- include in presentation
- where to store it? git? (license?)

### Automatic

- include image in presentation
- auto: download and resize
