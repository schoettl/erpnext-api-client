{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.DocTest

main :: IO ()
main = do
  doctest ["src/"]
