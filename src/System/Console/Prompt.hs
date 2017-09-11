{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Console.Prompt
    ( Prompt
    , newPrompt
    , execPrompt
    , failPrompt
    , parse
    , auto
    , str
    , choice
    , boundedEnum
    , yesno
    , module System.Console.Prompt.Settings
    ) where

import Control.Monad.Trans.Except
import Data.List (find, intersperse)
import Data.Semigroup ((<>))
import Data.String (IsString(..))
import Text.Read (readEither)

import System.Console.Prompt.Settings

-- | Composable action that represents prompting the user, and reading input
newtype Prompt a = Prompt { unPrompt :: ExceptT String IO a }
    deriving (Functor, Applicative, Monad)

-- | Construct a new Prompt from a prompting action
newPrompt :: IO (Either String a) -> Prompt a
newPrompt = Prompt . ExceptT

-- | Execute a prompt
execPrompt :: Prompt a -> IO (Either String a)
execPrompt = runExceptT . unPrompt

failPrompt :: String -> Prompt a
failPrompt = Prompt . ExceptT . return . Left

-- | Prompt and parse the read value with given function
parse :: (String -> Either String a) -> Mod -> Prompt a
parse p m = Prompt $ ExceptT $ maybe (Left "no input") p <$> runInput m

-- | Prompt for any @'Read' a => a@
auto :: Read a => Mod -> Prompt a
auto = parse readEither

-- | Prompt for any @'IsString' a => a@
str :: IsString a => Mod -> Prompt a
str = parse $ Right . fromString

-- | Prompt for a @'Bool'@, using @y/n@ as the choices
yesno :: Mod -> Prompt Bool
yesno = choice (cond 'y' 'n') [True, False]
  where
    cond :: a -> a -> Bool -> a
    cond c a p = if p then c else a

-- | Prompt for any bounded enumeration
boundedEnum :: (Enum a, Bounded a, Show a) => Mod -> Prompt a
boundedEnum = choice (head . show) [minBound..maxBound]

-- | Prompt for a choice among a list of values
choice :: (a -> Char) -> [a] -> Mod -> Prompt a
choice to els = parse p . (<> relabel options)
  where
    p (c:_) = maybe (Left "invalid input") Right $ find ((== c) . to) els
    p [] = Left "no input"
    options = (++ concat ["(", intersperse '/' $ map to els, ")? "])
