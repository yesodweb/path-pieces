{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck(prop)
import Test.QuickCheck

import Web.PathPieces
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.Time (Day(..))

-- import FileLocation (debug)

instance Arbitrary T.Text where
  arbitrary = fmap T.pack arbitrary

instance Arbitrary Day where
  arbitrary = fmap ModifiedJulianDay arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PathPiece" $ do
    prop "toPathPiece <=> fromPathPiece String" $ \(p::String) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece String" $ \(p::T.Text) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> T.null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece String" $ \(p::Int) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> p < 0
        Just pConverted -> p == pConverted

  describe "PathMultiPiece" $ do
    prop "toPathMultiPiece <=> fromPathMultiPiece String" $ \(p::[String]) ->
      p == (fromJust . fromPathMultiPiece . toPathMultiPiece) p

    prop "toPathMultiPiece <=> fromPathMultiPiece String" $ \(p::[T.Text]) ->
      p == (fromJust . fromPathMultiPiece . toPathMultiPiece) p

    prop "toPathMultiPiece <=> fromPathMultiPiece Integer" $ \(p::[Integer]) ->
      case (fromPathMultiPiece . toPathMultiPiece) p of
        Nothing -> any (< 0) p
        Just pConverted -> p == pConverted

    prop "toPathMultiPiece <=> fromPathMultiPiece Int" $ \(p::[Int]) ->
      case (fromPathMultiPiece . toPathMultiPiece) p of
        Nothing -> any (< 0) p
        Just pConverted -> p == pConverted

    prop "toPathMultiPiece <=> fromPathMultiPiece Int64" $ \(p::[Int64]) ->
      case (fromPathMultiPiece . toPathMultiPiece) p of
        Nothing -> any (< 0) p
        Just pConverted -> p == pConverted

    prop "toPathMultiPiece <=> fromPathMultiPiece Day" $ \(p::[Day]) ->
      p == (fromJust . fromPathMultiPiece . toPathMultiPiece) p


  describe "SinglePiece" $ do
    prop "toPathPiece <=> fromPathPiece String" $ \(p::String) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece String" $ \(p::T.Text) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> T.null p
        Just pConverted -> p == pConverted

    prop "toPathPiece <=> fromPathPiece String" $ \(p::Int) ->
      case (fromPathPiece . toPathPiece) p of
        Nothing -> p < 0
        Just pConverted -> p == pConverted

  describe "MultiPiece" $ do
    prop "toPathMultiPiece <=> fromPathMultiPiece String" $ \(p::[String]) ->
      p == (fromJust . fromPathMultiPiece . toPathMultiPiece) p

    prop "toPathMultiPiece <=> fromPathMultiPiece String" $ \(p::[T.Text]) ->
      p == (fromJust . fromPathMultiPiece . toPathMultiPiece) p
