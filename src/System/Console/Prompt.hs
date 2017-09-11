{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Console.Prompt
    ( Prompt
    , execPrompt
    , PromptSettings
    , Mod
    , label
    , relabel
    , input
    , masked
    , maskedBy
    , hidden
    , parse
    , auto
    , str
    , choice
    , boundedEnum
    , yesno
    ) where

import Control.Monad.Trans.Except
import Data.List (find, intersperse)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import System.Console.Haskeline
import Text.Read (readEither)

-- | Composable action that represents prompting the user, and reading input
newtype Prompt a = Prompt { unPrompt :: ExceptT String IO a }
    deriving (Functor, Applicative, Monad)

execPrompt :: Prompt a -> IO (Either String a)
execPrompt = runExceptT . unPrompt

-- | Settings describing how to present a prompt and read raw input
data PromptSettings = PromptSettings
    { psLabel :: String
    , psInput :: String -> InputT IO (Maybe String)
    }

defaultPromptSettings :: PromptSettings
defaultPromptSettings = PromptSettings
    { psLabel = "Input: "
    , psInput = getInputLine
    }

-- | Composable operation for modifying prompt settings
data Mod = Mod (PromptSettings -> PromptSettings)

instance IsString Mod where
    fromString = label

instance Semigroup Mod where
    (Mod a) <> (Mod b) = Mod $ b . a

-- | Set a prompts label, automatically appending @": "@
label :: String -> Mod
label = label' . (++ ": ")

-- | The same as @'label'@, but uses the content exactly as-is
label':: String -> Mod
label' s = Mod $ \p -> p { psLabel = s }

-- | Adjust an existing label, see @'choice'@ for an example use-case
relabel :: (String -> String) -> Mod
relabel f = Mod $ \p -> p { psLabel = f $ psLabel p }

-- | Change the input-reading function, e.g. to use @'getPassword'@
input :: (String -> InputT IO (Maybe String)) -> Mod
input f = Mod $ \p -> p { psInput = f }

-- | Change input-reading function to mask with @\'*\'@.
masked :: Mod
masked = maskedBy '*'

-- | Change input-reading function to mask with the given character
maskedBy :: Char -> Mod
maskedBy = input . getPassword . Just

-- | Change input-reading function to completely hide typed input
hidden :: Mod
hidden = input $ getPassword Nothing

-- | Prompt and parsing the read value with given function
parse :: (String -> Either String a) -> Mod -> Prompt a
parse p (Mod f) =
    let (PromptSettings l i) = f defaultPromptSettings
    in Prompt $ ExceptT $ maybe (Left "no input") p <$> runInputT defaultSettings (i l)

-- | Prompt for any @'Read' a => a@
auto :: Read a => Mod -> Prompt a
auto = parse readEither

-- | Prompt for any @'IsString' a => a@
str :: IsString a => Mod -> Prompt a
str = parse $ Right . fromString

-- | Prompt for a choice between values, see @'yesno'@ for an example
choice :: (a -> Char) -> [a] -> Mod -> Prompt a
choice to els = parse p . (<> relabel options)
  where
    p (c:_) = maybe (Left "invalid input") Right $ find ((== c) . to) els
    p [] = Left "no input"
    options = (++ concat ["(", intersperse '/' $ map to els, ")? "])

-- | Prompt for any bounded enumeration
--
-- Uses the first letter of the type's @'Show'@ instance for options, this means
-- it must be unique.
--
boundedEnum :: (Enum a, Bounded a, Show a) => Mod -> Prompt a
boundedEnum = choice (head . show) [minBound..maxBound]

-- | Prompt for a @'Bool'@ with @y@ or @n@ as the options
yesno :: Mod -> Prompt Bool
yesno = choice (cond 'y' 'n') [True, False]
  where
    cond :: a -> a -> Bool -> a
    cond c a p = if p then c else a
