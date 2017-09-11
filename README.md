# console-prompt

Declarative prompting in console applications.

If you enjoy [optparse-applicative][], you will probably like this library.

[optparse-applicative]: https://github.com/pcapriotti/optparse-applicative

## Quick Start

```hs
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Semigroup ((<>))
import System.Console.Prompt

data Registration = Registration
    { name :: String
    , age :: Int
    , password :: String
    , subscribe :: Bool
    }
    deriving Show

prompt :: Prompt Registration
prompt = Registration
    <$> str "Name"
    <*> auto "Age"
    <*> str ("Password" <> masked)
    <*> yesno "Subscribe"

main :: IO ()
main = print =<< execPrompt prompt
```

```console
% ./example
Name: Pat
Age: 32
Password: *****
Subscribe: (y/n)? n
Right (Registration ...
```

## From First Principals

Consider the following, lowest-level use of this library to ask someone's age:

```hs
agePrompt :: Prompt Int
agePrompt = parse                       -- (4)
    (\s ->                              -- (3)
        if all $ isDigit s
          then Right $ read s
          else Left "Invalid input"
    )
    (  label' "Age: "                   -- (2)
    <> input getInputLine               -- (1)
    )
```

Let's go through this expression bottom up, replacing lower-level constructs
with higher-level ones, to show how more pleasant usage is actually implemented
under the hood.

1. The second argument to `parse` is a "modifier" (of type `Mod`), it's
   basically a function from `PromptSettings -> PromptSettings` and will be
   applied to default settings to determine how the prompt works when executed.
   Multiple modifiers can be combined with `(<>)` (which is normal function
   composition).

   Reading terminal input is a complicated process, so this library leans
   entirely on the great `haskeline` package for this heavy-lifting. The `input`
   modifier is how you specify which `InputT` method to use.

   `getInputLine` does what you would expect. It also happens to be the default,
   let's remove it:

   ```hs
   agePrompt :: Prompt Int
   agePrompt = parse                       -- (4)
       (\s ->                              -- (3)
           if all $ isDigit s
             then Right $ read s
             else Left "invalid input"
       )
       (label' "Age: ")                    -- (2)
   ```

2. Here we've set the label. When using `label'`, the value will be used exactly
   as-is. This allows full control over formatting, but does require you are
   careful to append that trailing space. More common is to use `label`, which
   automatically appends `": "` after its argument. Since that's what we wanted
   here anyway, let's use that:

   ```hs
   agePrompt :: Prompt Int
   agePrompt = parse                       -- (4)
       (\s ->                              -- (3)
           if all $ isDigit s
             then Right $ read s
             else Left "invalid input"
       )
       (label "Age")
   ```

   Because this is the most common thing to do, the `IsString` instance for
   `Mod` assumes a string literal means to `label` with that string:

   ```hs
   agePrompt :: Prompt Int
   agePrompt = parse                       -- (4)
       (\s ->                              -- (3)
           if all $ isDigit s
             then Right $ read s
             else Left "invalid input"
       )
       "Age"
   ```

3. For our parsing function, we've written a safer version of `read`. With a
   slight difference in error message, we could use `Text.Read.readEither` here:

   ```hs
   agePrompt :: Prompt Int
   agePrompt = parse                       -- (4)
       readEither
       "Age"
   ```

4. It would make sense that `parse readEither` should work for any `Read a`, and
   exactly such a function is available as `auto`:

   ```hs
   agePrompt :: Prompt Int
   agePrompt = auto "Age"
   ```

   Another similar function is `str`. It's just like `auto` but for `IsString
   a`. This library works well with `newtype`-happy use-cases, where you might
   do something like:

   ```hs
   newtype Password = Password Text
   ```

   To add more type-safety to the `Text` values in your program. With
   `GeneralizedNewtypeDeriving`, you can `derive IsString` and write directly:

   ```hs
   execPrompt $ str ("Password" <> masked) :: IO (Either String Password)
   ```

## The `Mod` Type

*TODO*: motivate and describe these.

- `label`
- `label'`
- `input`
- `masked`
- `maskedBy`
- `hidden`

## Ready-made `Prompt`s

*TODO*: motivate and describe these.

- `choice`
- `boundedEnum`
- `yesno`

### Complex Examples

*TODO*: motivate and describe these. Should these be library-provided?

```hs
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Semigroup ((<>))
import System.Console.Prompt

confirm :: Eq a => (Mod -> Prompt a) -> Mod -> Prompt a
confirm f m = do
    v1 <- f m
    v2 <- f $ m <> relabel (++ "(confirm) ")

    if v1 == v2
        then return v1
        else failPrompt "Values don't match"

main :: IO ()
main = do
    password <- execPrompt $ confirm str ("Password" <> masked)
    print (password :: Either String String)
```

```console
% ./example
Password: *******
Password: (confirm) *******
Right "hunter2"
```

```console
% ./example
Password: *******
Password: (confirm) ******
Left "Values don't match"
```

```hs
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Console.Prompt

retry :: Prompt a -> Prompt a
retry p = newPrompt go
  where
    go = either (const go) (return . Right) =<< execPrompt p

main :: IO ()
main = do
    age <- execPrompt $ retry $ auto "Age"
    print (age :: Either String Int)
```

```console
% ./example
Age: xyz
Age: xyz
Age: 21
Right 21
```

## Development & Test

```console
stack build --pedantic test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
