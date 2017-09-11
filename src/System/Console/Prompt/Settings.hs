module System.Console.Prompt.Settings
    ( PromptSettings
    , runInput
    , Mod
    , label
    , label'
    , relabel
    , masked
    , maskedBy
    , hidden
    ) where

import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import System.Console.Haskeline

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

runInput :: Mod -> IO (Maybe String)
runInput (Mod f) =
    let (PromptSettings l i) = f defaultPromptSettings
    in runInputT defaultSettings (i l)

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
