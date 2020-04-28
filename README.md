# Stack-based Clash environment

This is my Clash playground now. If you came from the [blog post], you can check out the tag [`blog`][tag-block] for the exact version of code I refered to there.

[blog post]: (https://dram.cf/p/clash-with-stack/)
[tag-block]: (https://github.com/dramforever/clash-with-stack/tree/blog)

## What's in here

I play with small snippets of Clash code here, but you are probably here for the setup. To use it, just copy code from this repo to your own project.

### Stack

This project's [Clash] HDL project setup uses [Stack] as the build tool. You can check these files for what I've done:

- `stack.yaml`
- `package.yaml`
- `.clashi`

Theses are set up so that the following use cases work as intended:

- `stack repl` and `stack build`: Building Clash code as normal Haskell
    - The configuration sets up the GHC plugins and extensions needed
- `stack repl --with-ghc clash`: Running the Clash REPL (You can generate HDL from there)

[Clash]: https://clash-lang.org/
[Stack]: https-://docs.haskellstack.org/en/stable/README/

### Constraint trick with `-XPartialTypeSignatures`

Tired of writing all the `(HiddenClockResetEnable dom, NFDataX a, Num a, Eq a)` constraints? Just enable `-XPartialTypeSignatures` and `-Wno-partial-type-signatures` then replace all your constraints with a `_`.

Before:

```haskell
foo ::
     ( HiddenClockResetEnable dom
     , Num a, Default a, NFDataX a
     )
    => a -> a -> a
    -> Signal dom a
    -> Signal dom a
```

After:

```haskell
foo :: _
    => a -> a -> a
    -> Signal dom a
    -> Signal dom a
```

### `Synthesize` annotation trick

Have you ever found yourself writing `Sythesize` annotations like this:

```haskell
{-# ANN f Synthesize
    { t_name   = "f"
    , t_inputs = [ PortName "a"
                 , PortProduct "" [ PortName "b", PortName "c" ] ]
    , t_output = PortProduct "res" [PortName "q"]
    } #-}
```

And thought, maybe there's a less verbose way. Well, look no further than `-XOverloadedLists` and `-XOverloadedStrings`

```haskell
instance IsString PortName where
    fromString = PortName

instance IsList PortName where
    type Item PortName = PortName

    fromList = PortProduct ""

    toList = error "toList for PortName is not implemented"
```

Now we can just write:

```haskell
{-# ANN f Synthesize
    { t_name   = "f"
    , t_inputs = [ "a", [ "b", "c" ] ]
    , t_output = PortProduct "res" [ "q" ]
    } #-}
```

Isn't that just like, way more readable?

Note that these are indeed orphans so you might want to put `{-# OPTIONS -Wno-orphans #-}` in whatever file you are defining these instances.
