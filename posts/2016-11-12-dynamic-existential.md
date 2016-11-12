---
title: Phantom cats in a dynamical world
author: Călin Ardelean
tags: haskell, dynamic, types
---

The _Criticism_ section of the Wikipedia article on [late binding][wiki-late] states:

> Late binding necessarily prevents the use of static type checking.

That may be true, but traditional dynamic languages go further and enforce all
bindings to be dynamic, deporting static types en masse.
More recently, a movement known as "gradual typing" seeks to reverse this policy,
with systems such as [Typed Racket] reintroducing some (weak) static types to a
dynamic language built on top of a more sophisticated compiler infrastructure.

In this post we'll do the converse.
We will see how a compiled language with a sufficiently powerful static type system,
in our case _GHC Haskell_, can accommodate a fully dynamic meta environment
that supports "real" live coding.
We will use abstract data types implemented with the help of existential
quantification to solve the module dependency problem that would otherwise
force us to restart the world after every change and recompilation
of either entity declaration modules, or world behaviors (eg: physics engine).
We will also encounter phantom cats.

---

Let's start with a 3 module setup:

- `Word` will hold our global state together with the abstract glue
- `Physics` will contain generic world behaviors
- `Cat` will be our example entity

`Physics` and `Cat` will import `World` and we will be able to independently
make modifications and dynamically reload them in `ghci` without rebooting the world.

---

## The world

This is how our world might look:

```Haskell
{-# LANGUAGE ExistentialQuantification, RankNTypes, GeneralizedNewtypeDeriving #-}

module World (Entity(..), Ref, spawn, withEntity, foldWorld) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import qualified Data.IntMap.Strict as Map

class Entity a where
  health :: a -> Int
  attack :: Int -> a -> a

data EntityRecord = forall a. Entity a => EntityRecord a

world :: MVar (Map.IntMap EntityRecord)
world = unsafePerformIO $ newMVar Map.empty
```

`Entity` is our abstract entity class with a sample getter and setter inside.

`EntityRecord` existentially wraps a pointer to a value of a type implementing
the `Entity` interface, *together with the dictionary* of functions that any
future version of us can use to interact with it abstractly, as we shall see.

We put all entities inside the global, thread safe, heterogeneous `world`
map that will maintain our late bindings.

We also need an (internal) unique ID supply for the keys inside:

```Haskell
newtype Ref = Ref { getRef :: Int } deriving Num

instance Show Ref where show (Ref k) = show k

ref :: MVar Ref
ref = unsafePerformIO . newMVar $ Ref 0
```

The generic `spawn` function will be used in a context where we have a specific
entity type and its accompanying `Entity` instance.

```Haskell
spawn :: Entity a => a -> IO Ref
spawn e = do
  Ref k <- takeMVar ref
  let r = Ref (k + 1)
  putMVar ref (r `seq` r)
  w <- takeMVar world
  putMVar world $ Map.insert (getRef r) (EntityRecord e) w
  return r
```

The next 2 functions allow us to work generically with the world:

```Haskell
withEntity :: Ref -> (forall a. Entity a => a -> (a, b)) -> IO (Maybe b)
withEntity (Ref k) f = do
  w <- takeMVar world
  let (w', mb) = case Map.lookup k w of
                   Just (EntityRecord e) -> let (e', x) = f e in
                                            (Map.insert k (EntityRecord e') w, Just x)
                   Nothing -> (w, Nothing)
  putMVar world w'
  return mb

foldWorld :: Monoid m => (forall a. Entity a => Ref -> a -> m) -> IO m
foldWorld f = readMVar world >>= return . Map.foldMapWithKey f'
  where f' k (EntityRecord e) = f (Ref k) e
```

`withEntity` does a lookup based on a `Ref` and applies a caller supplied
generic entity update function that also returns a `b`.
Hence, the argument this function receives will be "late bound".

---

We use here `RankNTypes`.
In case you see this for the first time it's important to understand who,
among the caller and callee, has the responsibility to supply each argument
in a nested, polymorphic function type.
If it's the callee we call such location in the shape of the type (like the
return type of the whole function) **positive**.
And if it's the caller, **negative**.
It so happens that _School of Haskell_ has published a [comprehensive
article][variance] on this very topic just as I was writing this.
Lucky me.

After understanding variance we should observe that we are not talking here
about who supplies values at run time, but rather who supplies type arguments
at type checking time.
But the analogy carries well when we remember that in _System F_ (the basis for
_Haskell Core_), type applications are explicit, and even more, in dependently typed
languages all global distinctions between types and values melt away.

In _Core_, functions actually have extra lambda bindings for their type arguments,
and these lambdas can appear in positive or negative positions just as before,
except that the type applications can be evaluated at compile time,
and all types eventually erased due to the limitations imposed in the language.

In old school parametric polymorphism all types are rank 1, meaning it's always
the caller that fills in type arguments, regardless of the variance of
the (run time) arguments that have those types.
So all big lambdas float to the top of a nested function, while small lambdas
can get stuck submerged in a negative position.

Combining the two concepts, we see that a *negative* type argument afforded by
a higher rank type not only that it forwards the whodunit question from the caller
to the callee, but also from compile time to run time, in terms of
**when can we compile** the specialized version of the function.
This would be a sort of type theoretic explanation of JIT compilation.
For the moment a very confused one, I'll admit.
As you may have guessed, I'm no expert.

---

Returning to `withEntity`, both the `a` and the `Entity a` instance are
chosen by the calee, ourselves, and we will pull those out of our `world` map.

The `foldWorld` function does a similar job as the `foldMap` from `Traversable`,
but uses the above trick to do it abstractly.

---

## The entity

In the situation of losing all health, a non-black cat will turn black,
and after that any attack will make it more powerful:

```Haskell
module Cat where

import World

data Cat = Cat { catHealth :: Int, blackCat :: Bool }

instance Entity Cat where
  health = catHealth
  attack p (Cat h b) = if b then Cat (h + p) b
                            else if p > h then Cat 0 True
                                          else Cat (h - p) b
```

---

## Physics

In our `Physics.hs` file we will only have a simple generic world tracing function.
Notice how we do not import the `Cat`, and how the type system ensures that
the function `f` we supply to `foldWorld` only uses the `Entity` interface
abstractly:

```Haskell
module Physics (inspectWorld) where

import World

inspectWorld :: IO String
inspectWorld = foldWorld f
  where f r e = "(id: " ++ show r ++ ", health: " ++ show (health e) ++ ") "
```

One could argue that this abstraction is a tremendous limitation concerning
what kinds of generic traversals we can write, that we would have to keep adding
methods to `Entity`, which do require rebooting the world.

But, at least in the case of video games, keeping a fixed, simple, generic
type for entities is exactly what people do anyway, for many reasons.
For instance, a physics engine, much like a garbage collector, does not care
about the complex structure and behaviors of the objects it manages, but only
about their mass and shape, mixing in properties like velocity and acceleration
in its own private memory.

Another answer can be this Alan Perlis quote LISP people like to deploy when cornered:

> It is better to have 100 functions operate on one data structure than 10 functions on 10 data structures.

Nevertheless, we could extend the simple setup described here with one of the
solutions to the expression problem, like the `mtl` style, data types à la carte,
or the `Free` / `Cofree` [pairing][piponi].
Exploring this is left as an exercise for the industrious reader.
Which includes me, I hope.

---

## Doing it live

I should point out that the code in this post can also be found on [Github].

So let's fire up `stack ghci` and play.

The first thing we will do is activate `-fobject-code`, which allows `ghci`
to use compiled, rather then interpreted code, albeit without `-O2` optimizations
(but everything works with interpreted code as well).
More, for all libraries we use, `ghci` will link against their statically
or dynamically compiled versions (including the optimizations).
So we can use a multi package setup for our game to arbitrate this convenience
vs. performance trade off.
Additionally, we'll hope `ghci` will apply the `-O2`s in the future.

```ghci
λ> :set -fobject-code 
λ> :reload
```

Let's spawn a cat, send an attack message, and inspect the world:

```ghci
λ> spawn $ Cat 100 False
1
λ> withEntity 1 $ \cat -> (attack 10 cat, health cat)
Just 100
λ> inspectWorld
"(id: 1, health: 90) "
```

All fine. Now let's open our `Cat.hs` file, change how attacks work by making
them twice as damaging, then save the file:

```Haskell
instance Entity Cat where
  health = catHealth
  attack p (Cat h b) = if b then Cat (h + p) b
                            else if p > h then Cat 0 True
                                          else Cat (h - 2*p) b
```

We now switch back to our `ghci` terminal and `:reload` (I formatted the output a bit):

```ghci
λ> :reload
[3 of 4] Compiling Cat  ( /home/calin/src/dynamic-meta/src/Cat.hs,
                          /home/calin/src/dynamic-meta/.stack-work/odir/Cat.o )
Ok, modules loaded:
World (/home/calin/src/dynamic-meta/.stack-work/odir/World.o),
Physics (/home/calin/src/dynamic-meta/.stack-work/odir/Physics.o),
Cat (/home/calin/src/dynamic-meta/.stack-work/odir/Cat.o)
```

`ghci` has recompiled our `Cat.hs` and has placed the resulting object file in a
subfolder called `odir` of our `stack` working environment.

Now let's check the world:

```ghci
λ> inspectWorld 
"(id: 1, health: 90) "
```

Interesting. Our old cat is still there, even if neither the source file
defining its internals, nor the compiled object file still exist.
A phantom cat!

Let's see if we can add a new cat and test how both it and the phantom behave
when attacked:

```ghci
λ> spawn $ Cat 100 False
2
λ> withEntity 1 $ \cat -> (attack 10 cat, health cat)
Just 90
λ> withEntity 2 $ \cat -> (attack 10 cat, health cat)
Just 100
λ> inspectWorld
"(id: 1, health: 80) (id: 2, health: 80) "
```

Our new cat takes a 20 hit for a 10 attack, as expected, but the phantom still
takes 10 as before!

So there it is, 2 versions of the "same" object type, together with their
methods, both compiled, coexisting in peace and type safety inside the same heap.
If this does not make both Alan Kay and Robert Harper happy, I don't know what will.
I'm joking, of course.
Nothing can restore those guys' faith in humanity after what we did with their ideas.

We can also make changes to the `Physics`:

```Haskell
inspectWorld :: IO String
inspectWorld = foldWorld f
  where f r e = "(id = " ++ show r ++ ", health = " ++ show (health e) ++ ") "
```

...reload, and check if the world still exists:

```ghci
λ> :reload
[3 of 4] Compiling Physics ( /home/calin/src/dynamic-meta/src/Physics.hs,
                             /home/calin/src/dynamic-meta/.stack-work/odir/Physics.o )
Ok, modules loaded:
World (/home/calin/src/dynamic-meta/.stack-work/odir/World.o),
Physics (/home/calin/src/dynamic-meta/.stack-work/odir/Physics.o),
Cat (/home/calin/src/dynamic-meta/.stack-work/odir/Cat.o).
λ> inspectWorld
"(id = 1, health = 80) (id = 2, health = 80) "
```

---

## Final remarks

If we consider those `Ref`s as variables, with objects holding dynamic
references to each other (by `Ref`), forcing us to use something like
`withEntity` all the time, then of course we have a late binding
implementation that is untyped, unsafe, slow, and in general suffering from
all the ills mentioned in that wiki article ("entity not found", etc).

Just like in _Racket_, we can unfold some macros, sub types and contracts
to tame them.
But we could also have objects hold normal, immutable pointers to each other
that only get updated once per frame, automatically.
I'll have to think about this more.

---

GHC's in-house linker that makes all this possible, I have no clue about
what it does.
Tis magic.

---

GHC has a type called `Dynamic` that cannot be used for live coding
to the same extent, since opening a `Dynamic` inside a generic world traversal
will introduce a module dependency to the entity definition module.
So I think `Dynamic` does not live up to its name,
at least not in [Alan Key's][smalltalk] [sense][dynamic].

---

As for me, I'll go read that Reynolds "GEDANKEN" paper Alan Key mentions,
and further explore the connection between "dynamic" and "higher rank",
which undoubtedly will turn out to involve some trivial misunderstandings on my part.
But at least I'll have the motivation to learn more type theory.

[wiki-late]: https://en.wikipedia.org/wiki/Late_binding "Late binding - Wikipedia"
[Typed Racket]: https://docs.racket-lang.org/ts-guide/index.html "The Typed Racket Guide"
[variance]: https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance "Covariance, contravariance, and positive and negative position - School of Haskell"
[piponi]: http://blog.sigfpe.com/2014/05/cofree-meets-free.html "A Neighborhood of Infinity: Cofree meets Free"
[Github]: https://github.com/mmn80/dynamic-meta "Github repo containing the code from this post"
[smalltalk]: http://userpage.fu-berlin.de/~ram/pub/pub_jf47ht81Ht/doc_kay_oop_en "Dr. Alan Kay on the Meaning of “Object-Oriented Programming”"
[dynamic]: https://computinged.wordpress.com/2010/09/11/moti-asks-objects-never-well-hardly-ever/ "In the comments section of this blog post, Alan Key suggests using the term “dynamic” to capture what he meant by OOP before that term's hijacking"
