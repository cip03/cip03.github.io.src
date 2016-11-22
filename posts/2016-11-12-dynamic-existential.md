---
title: Phantom cats in a dynamic world
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
in our case _GHC Haskell_, can accommodate a dynamic meta environment
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

data SomeEntity = forall a. Entity a => SomeEntity a

world :: MVar (Map.IntMap SomeEntity)
world = unsafePerformIO $ newMVar Map.empty
```

`Entity` is our abstract entity class with a sample getter and setter inside.

`SomeEntity` [existentially] wraps a pointer to a value of a type implementing
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

newRef :: IO Ref
newRef = do
  Ref k <- takeMVar ref
  let r = Ref (k + 1)
  putMVar ref (r `seq` r)
return r
```

The generic `spawn` function will be used in a context where we have a specific
entity type and its accompanying `Entity` instance.

```Haskell
spawn :: Entity a => a -> IO Ref
spawn e = do
  r <- newRef
  w <- takeMVar world
  putMVar world $ Map.insert (getRef r) (SomeEntity e) w
  return r
```

The next 2 functions allow us to work generically with the world:

```Haskell
withEntity :: Ref -> (forall a. Entity a => a -> (a, b)) -> IO (Maybe b)
withEntity (Ref k) f = do
  w <- takeMVar world
  let (w', mb) = case Map.lookup k w of
                   Just (SomeEntity e) -> let (e', x) = f e in
                                          (Map.insert k (SomeEntity e') w, Just x)
                   Nothing -> (w, Nothing)
  putMVar world w'
  return mb

foldWorld :: Monoid m => (forall a. Entity a => Ref -> a -> m) -> IO m
foldWorld f = readMVar world >>= return . Map.foldMapWithKey f'
  where f' k (SomeEntity e) = f (Ref k) e
```

`withEntity` does a lookup based on a `Ref` and applies a caller supplied
generic entity update function that also returns a `b`.
Hence, the argument this function receives will be "late bound".

---

We use here [higher rank polymorphism][rankn].

The usual rank 1 parametric polymorphism that we find in Haskell 98 only allows
type quantifiers (`forall`s) at the beginning of a nested function's type.
When translated to the intermediate language _Core_, which is based on the
powerful _System F_, these `forall`s are replaced with extra arguments,
except that they are written with big `Λ`s, rather then small `λ`s, in order to
distinguish them as type arguments.
At the call site of a polymorphic function these arguments are explicitly
instantiated with a (monomorphic) type, and this type is computed at compile
time with the HM type inference algorithm.

Now, if more of the power of _System F_ is exposed through `-XRankNTypes`,
we can write functions that have deeply nested `forall`s, aka `Λ`s.
Type inference becomes undecidable, so the user has to provide explicit
signatures to help the type checker.
But notice how this means that it's no longer the caller's privilege to
always instantiate the type arguments.
Similarly, normal `λ` bindings can also appear in a [negative position][variance].

A negatively placed `Λ` means that this function accepts a polymorphic function
as argument, and gets to instantiate it (dynamically!) to whatever type it wants.
In particular, as a closure, it can have such a secret type in its environment.
With existentials, we wrap a type and a bunch of primitive operations together,
and then write rank 2 functions that receive from users a polymorphic function
that can only use those primitives, targeting the object it gets as argument
but whose type it'll never know.

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
That repo also showcases other methods for live coding, which I'll discuss in
future posts.

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
> :set -fobject-code 
> :reload
```

Let's spawn a cat, send an attack message, and inspect the world:

```ghci
> spawn $ Cat 100 False
1
> withEntity 1 $ \cat -> (attack 10 cat, health cat)
Just 100
> inspectWorld
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
> :reload
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
> inspectWorld 
"(id: 1, health: 90) "
```

Interesting. Our old cat is still there, even if neither the source file
defining its internals, nor the compiled object file still exist.
A phantom cat!

Let's see if we can add a new cat and test how both it and the phantom behave
when attacked:

```ghci
> spawn $ Cat 100 False
2
> withEntity 1 $ \cat -> (attack 10 cat, health cat)
Just 90
> withEntity 2 $ \cat -> (attack 10 cat, health cat)
Just 100
> inspectWorld
"(id: 1, health: 80) (id: 2, health: 80) "
```

Our new cat takes a 20 hit for a 10 attack, as expected, but the phantom still
takes 10 as before!

So there it is, 2 versions of the "same" object type, together with their
methods, both compiled, coexisting in peace and type safety inside the same heap.

We can also make changes to the `Physics`:

```Haskell
inspectWorld :: IO String
inspectWorld = foldWorld f
  where f r e = "(id = " ++ show r ++ ", health = " ++ show (health e) ++ ") "
```

...reload, and check if the world still exists:

```ghci
> :reload
[3 of 4] Compiling Physics ( /home/calin/src/dynamic-meta/src/Physics.hs,
                             /home/calin/src/dynamic-meta/.stack-work/odir/Physics.o )
Ok, modules loaded:
World (/home/calin/src/dynamic-meta/.stack-work/odir/World.o),
Physics (/home/calin/src/dynamic-meta/.stack-work/odir/Physics.o),
Cat (/home/calin/src/dynamic-meta/.stack-work/odir/Cat.o).
> inspectWorld
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

---

GHC's in-house linker that makes all this possible, I have no clue about
what it does.
Tis magic.

---

**cocreature** has a [blog post][cocreature] showing how to dynamically load
GHC compiled modules in general, as long as you do the bookkeeping.

[wiki-late]: https://en.wikipedia.org/wiki/Late_binding "Late binding - Wikipedia"
[Typed Racket]: https://docs.racket-lang.org/ts-guide/index.html "The Typed Racket Guide"
[existentially]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#existentially-quantified-data-constructors "9.1. Language options — Glasgow Haskell Compiler Users Guide"
[variance]: https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance "Covariance, contravariance, and positive and negative position - School of Haskell"
[rankn]: https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html "24 Days of GHC Extensions: Rank N Types"
[piponi]: http://blog.sigfpe.com/2014/05/cofree-meets-free.html "A Neighborhood of Infinity: Cofree meets Free"
[Github]: https://github.com/mmn80/dynamic-meta "Github repo containing the code from this post"
[cocreature]: https://purelyfunctional.org/posts/2016-05-20-dynamic-loading-haskell-module.html "purelyfunctional.org - Dynamic loading of Haskell modules"
