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
