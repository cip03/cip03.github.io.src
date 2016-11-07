---
title: Ghost cats in a dynamic world
author: Călin Ardelean
tags: haskell
---

Introduction
------------

The traditional n-stage static compilation method becomes a bit of a pain
when developing complex, dynamical, interactive simulations, like video games.
For these, "correctness" includes performance related constraints
(eg: poor FPS makes a game unplayable, just like if it would have crashed).
And the process of game design is a creative endeavor that may benefit from
embedding within a live playground.
So we want to live test as much as we can, but there may be lots of resources
that need to be expensively initialized before the world can bootstrap itself.

Languages in the __LISP__ family, like [Racket], [Guile] or [Clojure], offer
this kind of productivity enhancing runtime metaprogramming through features such as
late binding of variables, JIT compilation, dynamic polymorphism, and a small,
built in set of heterogeneous collection types (lists, vectors and maps).
Alan Key's work on __Smalltalk-80__, __Squeak__, and the more recent
__COLA__/__OMeta__ system developed at the [Viewpoints Institute]
focuses entirely on this "language as operating system" paradigm.

Oh the other hand, we have the __ML__ world spawned from the theorem proving
research community, offering a separate order of magnitude productivity
enhancing drug: the static, ever more stronger, types.
In the dependent, linear, or session incarnations, as [Conor McBride] points out,
the types have started turning the tables, they now want to write the
boring parts of our programs for us.
With __Haskell__, the combination of static types and purity has also enabled
the creation of an optimizing compiler that produces very fast machine code.

Surely, we want both of these drugs. But there seems to be a basic trade off here.
Dynamic metaprogramming languages always come packaged with weak, dynamic types.
They appear to be doing it on purpose.
For instance, on the Clojure web page we read:
"It is better to have 100 functions operate on one data structure
than to have 10 functions operate on 10 data structures.",
which flies in the face of my Haskelly forged intuitions.
I would rather write 20 lines of Haskell then 101 lines of LISP.

In this post, I will show how, by using existentially quantified types, we can
solve the dependency problem that would otherwise force us to reboot the world
after every change in the entity types.
We will build a simple game world that allows live, dynamic recompilation of both
entities and generic world behaviors (like a physics engine),
all working within `ghci`.
We will also encounter ghost cats.

[Racket]: http://racket-lang.org/ "The Racket Language"
[Guile]: https://www.gnu.org/software/guile/ "GNU's programming and extension language — GNU Guile"
[Clojure]: http://clojure.org/about/rationale "Clojure - Rationale"
[Viewpoints Institute] http://vpri.org/ "Viewpoints Research Institute"
[Conor McBride]: https://skillsmatter.com/skillscasts/8893-is-a-type-a-lifebuoy-or-a-lamp "Is a type a lifebuoy or a lamp?"
