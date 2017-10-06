## Description

This is my 'mix' for an 'Advanced' Scala Workshop at the job site. Based on Scala documents, articles, some papers, stackoverflow answers, ... and some own 'visions' / tips.

**Editions**:

  - 28/09/2017 at DT

**Includes**:

  - slides
  - coding playground

## Index

    - The Type System and its parts
    - Typed Functional Programming
    - CT Abstractions
    - Dependency Injection
    - Immutable Data Structures Management with Optics
    - Function-al Design
    - Final

## Goals

    - Having fun coding Scala
    - How to use Types to get better code: \*- bugs & + maintainable
    - How to use Functional Programming to get code: \*+ reasonable & + composable & - bugs
    - Improving our Abstraction skills

## Running

For playing slides:

```
$ sbt console
:load "./slides/REPLesent.scala"
// with resolution 1280 x 1024
val replesent = REPLesent(105, 35, source="./slides", intp=$intp, slideCounter=true, slideTotal=true); import replesent._
h
first
```

To run examples:

```
$ sbt run
```

## Exploring

Explore and hack gratefully with Ammonite Repl:

```
$ amm
import $exec.play
```

## Credits

- Replesent project: https://github.com/marconilanna/REPLesent
- And all the authors and projects used in this project (listed in references)
