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
