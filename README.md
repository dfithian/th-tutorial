# Template Haskell Workshop

This is the repository for a Template Haskell workshop. See [the proposal](doc/proposal.md) for the original
inspiration.

## Setup

[Markdown Unlit](https://hackage.haskell.org/package/markdown-unlit) is a GHC plugin for rendering Markdown as Literate
Haskell. Exercises in this workshop are written in Markdown, so we need to install the package.

```bash
stack install markdown-unlit
```

The `src` folder contains "solved" functions which will be necessary for completing the exercises.

```bash
stack build --fast # compile the solved functions
```

## Exercises

The first two exercises are exploratory in GHCi to get a feel of how Template Haskell works. The next exercises go into
more detail about how to define and test generated code.

* [Exercise01](exercises/Exercise01.md) explores "hello world" in TH
* [Exercise02](exercises/Exercise02.md) explores nuances in TH types
* [Exercise03](exercises/Exercise03.md) helps create helper functions for inspection of a type's constructors at compile
  time.
* [Exercise04](exercises/Exercise04.md) uses the helper functions to generate instances for an enumeration.
* [Exercise05](exercises/Exercise05.md) demonstrates how to invoke the instance generation function.
* [Exercise06](exercises/Exercise06.md) demonstrates how to test the generated instances in Hspec.

Start in the `exercises` folder and fill in the `TODO` parts of each exercise.
