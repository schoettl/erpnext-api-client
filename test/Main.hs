{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ERPNext.Client
import ERPNext.Client.Helper
import ERPNext.Client.QueryStringParams
import Test.DocTest
import Data.Text
import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

main :: IO ()
main = do
  doctest ["src/"]
  hspec $ do
   describe "ERPNext.Client.QueryStringParams" $ do
    describe "renderQueryStringParams" $ do
      it "escapes quote and comma" $ do
        renderQueryStringParams [Asc "\","] `shouldBe` "order_by=%22%2C%20asc"
      it "escapes even brackets" $ do
        renderQueryStringParams [Fields []] `shouldBe` "fields=%5B%5D"
   describe "ERPNext.Client.Helper" $ do
    describe "urlEncode/urlDecode" $ do
      it "is reversable" $ hedgehog $ do
        t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
        (urlDecode . urlEncode) t === t
    describe "quote" $ do
      it "is always surrounded by double quotes" $ hedgehog $ do
        t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
        let quoted = quote t
        ("\"" `isPrefixOf` quoted) === True
        ("\"" `isSuffixOf` quoted) === True
      it "never returns something with double quotes inside" $ do
        quote "\"end" `shouldBe` "\"\\\"end\""
        quote "Mutter 2,5\"" `shouldBe` "\"Mutter 2,5\\\"\""
   describe "Internal helper functions" $ do
    describe "removeFirstLastChar" $ do
      it "seems to work" $ do
        removeFirstLastChar "abcd" `shouldBe` "bc"
        removeFirstLastChar "abc" `shouldBe` "b"
        removeFirstLastChar "ab" `shouldBe` ""
        removeFirstLastChar "a" `shouldBe` ""
        removeFirstLastChar "" `shouldBe` ""
      it "strips two characters" $ hedgehog $ do
        t <- forAll $ Gen.text (Range.linear 2 100) Gen.unicode
        let result = removeFirstLastChar t
        T.length result === T.length t - 2
   describe "Secret doesn't expose itself with show" $ do
     it "never shows" $ hedgehog $ do
        t <- forAll $ Gen.text (Range.linear 2 100) Gen.unicode
        show (mkSecret t) === "*****"

removeFirstLastChar :: Text -> Text
removeFirstLastChar = T.reverse . T.drop 1 . T.reverse . T.drop 1
