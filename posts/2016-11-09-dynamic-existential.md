---
title: Phantom cats in a dynamical world
author: Călin Ardelean
tags: haskell
---

The _Criticism_ section of the Wikipedia page on [late binding][wiki-late] states:

> Late binding necessarily prevents the use of static type checking.

In this post we will use existential quantification to unfold a concrete
implementation of dynamic binding in GHC Haskell, that is both (JIT) compiled,
and type safe (with caveats).
We will build a simple game world that allows dynamic recompilation of
entities and generic world behaviors (like a physics engine),
all working within `ghci`.
We will also encounter phantom cats.

---

Let's start with a 3 module setup:

- `Word` will contain our global state together with the magical dynamic glue
- `Physics` will contain generic world behaviors
- `Cat` will be our example entity

`Physics` and `Cat` will import `World` and we will be able to independently
make modifications and dynamically reload them without rebooting the world.

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

`Entity` is our abstract entity class with an example getter and setter inside.

`EntityRecord` existentially wraps a pointer to a type implementing
the `Entity` interface, *together with the dictionary* of functions that we can
use to interact with it abstractly.
This is our late binding.

We then put all entities inside the global, thread safe, heterogeneous `world` map.

We also need an (internal) unique ID supply for the keys inside our `IntMap`:

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
generic entity update function, that also returns a `b`.

---

We use here `RankNTypes`.
In case you see this for the first time it's important to understand who,
among the caller and callee, has the responsibility to supply each type parameter
in a nested, polymorphic function type.
If it's the callee we call such location in the shape of the type (like the
return type of the whole function) **positive**.
And if it's the caller, **negative**.
Hope I didn't mangle this.

In old school parametric polymorphism all types are rank-1, meaning it's always
the caller that fills in type arguments.
Crucially, a *negative* type argument afforded by a higher rank type not only
that it moves the **who** decision **from the caller to the callee**,
but also **from compile time to run time**, in terms of **when** can we
specialize the compiled function.
Existential types capture the very essence of "dynamic" (binding, linking, etc).
I will continue this "higher rank types vs. operating systems" nonsense after
we complete our `ghci` demo.

---

Returning to `withEntity`, both the `a` and the `Entity a` instance are
chosen by the calee, ourselves, and we will pull those out of our `world` map.
Note how this is both dynamic and type safe.

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

In our `Physics.hs` file we will only have a simple generic tracing function.
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

One could argue that this abstraction is a great limitation in terms of what
kinds of generic traversals we can write, that we would have to keep adding
methods to `Entity`, which do require rebooting the world.

But, at least in the case of video games, keeping a fixed, simple, generic
type for entities is exactly what people do anyways, for many reasons.
For instance, a physics engine, much like a garbage collector, does not care
about the complex behaviors of the objects it manages, but only about their
mass and shape, mixing in properties like velocity and acceleration in its own
local memory.

Another answer can be this Alan Perlis quote LISP people like to deploy when cornered:

> It is better to have 100 functions operate on one data structure than 10 functions on 10 data structures.

Nevertheless, we could extend the simple setup of this post with one of the
solutions to the expression problem, like the `mtl` style, data types à la carte,
or the `Free` / `Cofree` [pairing][piponi].
Exploring this is left as an exercise for the industrious reader.
Which includes me, I hope.

---

## Doing it live

I should point out that the code in this post can be found on [Github].

So let's fire up `stack ghci` and play.

The first thing we will do is activate `-fobject-code`, which allows `ghci`
to use compiled, rather then interpreted code, albeit without `-O2` optimizations.
But it works the same with interpreted code.
Also note that for all libraries we use, `ghci` will link against their statically
or dynamically compiled versions (including the optimizations).
So we can use a multi package setup for our game to optimize this convenience
vs. live performance trade off, and we'll hope `ghci` will use the `-O2` in
the future.

```ghci
λ> :set -fobject-code 
λ> :reload
```

Let's spawn a cat, send an attack message, and inspect the world:

```
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

We now switch back to our `ghci` terminal and `:reload` (I formated the output a bit):

```
λ> :reload
[3 of 4] Compiling Cat  ( /home/calin/src/dynamic-meta/src/Cat.hs,
                          /home/calin/src/dynamic-meta/.stack-work/odir/Cat.o )
Ok, modules loaded:
World (/home/calin/src/dynamic-meta/.stack-work/odir/World.o),
Physics (/home/calin/src/dynamic-meta/.stack-work/odir/Physics.o),
Cat (/home/calin/src/dynamic-meta/.stack-work/odir/Cat.o)
```

`ghci` has recompiled our `Cat.hs` and has put the resulting object file in a
subfolder called `odir` of our `stack` working environment.

Now let's check the world:

```
λ> inspectWorld 
"(id: 1, health: 90) "
```

Interesting. Our old cat is still there, even if neither the source file
defining its behavior, nor the compiled object file exist any more.
A phantom cat!

Let's see if we can add a new cat and test how both it and the phantom behave
when attacked:

```
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
methods, both compiled, coexisting in peace and type safety in the same heap.
If this does not make both Alan Kay and Robert Harper happy, I don't know what will.

We can also make changes to the `Physics`:

```Haskell
inspectWorld :: IO String
inspectWorld = foldWorld f
  where f r e = "(id = " ++ show r ++ ", health = " ++ show (health e) ++ ") "
```

...reload, and check is the world still exists:

```
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

## Lies and scandal

If we consider those `Ref`s as variables, with objects holding dynamic
references to each other (by `Ref`), then of course we have a late binding
implementation that is untyped, unsafe, slow, and in general suffers from
all the ills mentioned in that wiki article ("object not found", etc).

Just like in [Racket], we can implement some macros, dynamic types and contracts
to tame them.
But we could also have objects hold normal, immutable pointers to each other,
that only get updated once per frame.
Or maybe we could cook some higher rank scheme to make it even nicer.

I'll have to think about this more, but what has been showed still stands.

---

## Existential types <-> dynamic metaprogramming

I promised an elaboration on this connection.
Firstly, I should point out that, as you may have guessed, I'm neither a
type theory expert, nor a compilers or operating systems one.
So I'm just making trivial observations and asking stupid questions.

All we had here were actually rank 2 types, but what about going higher?
I think extending the analogy could mean something like having
phantom worlds (together with their compilers) within phantom worlds, etc.
Does this tree structure flatten out into a meta tower for some reason?
What's the connection with modal logic and Kripke semantics?
Does System F actually ensures consistency of this multiverse?
What complications do dependent types add to this story?
Ok, I'll stop here before I make too big a fool of myself.

But I would like to follow this thread further in the future, use it as a
motivator to learn new stuff.
I'd like to have a "compiler + runtime + internalized bootstrapping" for
type theory that automatically implements this "first class runtime
metaprogramming" interpretation of types.
I'm sure this is a pipe dream arising from some basic misunderstanding,
but I really want to know why exactly is this undecidable, inconsistent,
useless and makes baby cats cry.

[wiki-late]: https://en.wikipedia.org/wiki/Late_binding "Late binding - Wikipedia"
[Racket]: http://racket-lang.org/ "The Racket Language"
[Github]: https://github.com/mmn80/dynamic-meta "Github repo containing the code from this post"
[piponi]: http://blog.sigfpe.com/2014/05/cofree-meets-free.html "A Neighborhood of Infinity: Cofree meets Free"
