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
