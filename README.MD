## Description

This is a Scala language workshop, includes:

  - slides
  - coding playground

## Running

For playing slides:

$ sbt console

```
:load "./slides/REPLesent.scala"
// with resolution 1280 x 1024
val replesent = REPLesent(105, 35, source="./slides", intp=$intp, slideCounter=true, slideTotal=true); import replesent._
h
first
```


To run examples:

$ sbt run
