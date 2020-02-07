# Title

Template Haskell is for Haters - Of Boilerplate

# Format

Hop Workshop (2h)

# Audience

Intermediate

# Elevator Pitch

We all hate boilerplate code. Having to restate slightly different incantations of similar code over and over again is
not only mind numbing, it's prone to error. Use Template Haskell to kill boilerplate with fire.

# Description

Everyone can agree that writing boilerplate code sucks. The options available in the Haskell ecosystem to reduce
boilerplate are billed as "advanced" features - [Scrap Your Boilerplate](http://hackage.haskell.org/package/syb),
[Generic programming](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html), [Template Haskell
metaprogramming](http://hackage.haskell.org/package/template-haskell). However, a simple application of any of these
solutions can go a long way to improving code quality, reducing bugs, and increasing developer happiness. So why do they
have to be advanced features?

Template Haskell is more approachable than it appears, and this workshop will empower attendees to use it to their
advantage. In this workshop, we'll cover the "why" and "how" of basic applications of TH. Over the course of two hours,
attendees will get familiar with TH functions, identify some boilerplate code to refactor, rewrite it using vanilla TH
and QuasiQuoters, and write tests. By the end, attendees will have written something they can be proud of and be armed
with tools for diving deeper into the world of Template Haskell.

**NOTE** This talk expects the attendee to have used Haskell before, be used to working in the type system, and
understand concepts like monads.

# Notes

## Format

This talk will be divided into lecture sessions and exercises.

### Lecture topics

The introduction will center around the tools we'll need for the workshop, what exactly Template Haskell is, what the
`Q` monad is, and how can we use it? Exercises will include a simple "Hello World" written in TH, `fail`ing in `Q`, and
printing the output of a TH splice.

After getting familiar with the base Template Haskell tools, we'll jump into the larger exercise. Lecture will cover
identifying some boilerplate code, and introduce some basic combinators for TH needed for the exercise. The exercises
will cover rewriting the boilerplate code identified in TH.

After rewriting the boilerplate code, the next section will introduce QuasiQuoters. QuasiQuoters are a way to "lift"
regular Haskell code into Template Haskell's `Q` monad, so their purpose is to simplify the TH we have already written.
The exercises will simplify the TH just written in the previous exercise to use QuasiQuoters.

The final section will cover testing. I want to leave a lot of time for this because it's the most important. Nothing's
worse than writing an untestable macro! Lecture will cover the elements of successful tests and writing some tests.
Exercises will be to wire in the TH splices, iterate, and fix bugs.

## Motivation

Simply put, writing boilerplate code makes me very grumpy. For years I went about writing boring instances because I was
afraid of using Template Haskell. Since I decided to start learning to use Template Haskell, my team's use of Template
Haskell has empowered us to focus more on the real code that drives our company and industry forward. I want to share
that knowledge with others, because the more efficient a team or company is in Haskell the better it is for the
ecosystem as a whole.

Template Haskell is a means to greater productivity, because it takes care of mindless, error-prone work, and lets
developers focus on writing code crucial to the business. It shouldn't be scary.

# Tags

* Haskell
* Template Haskell
