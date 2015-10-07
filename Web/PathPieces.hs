-- |
-- Convert Haskell values to and from route pieces.
module Web.PathPieces (
  -- * Examples
  -- $examples

  -- * Classes
  PathPiece (..),
  PathMultiPiece (..),

  -- * Helpers
  readFromPathPiece,
  showToPathPiece,

  -- * Deprecated
  toSinglePiece,
  toMultiPiece,
  fromSinglePiece,
  fromMultiPiece,
) where

import Web.PathPieces.Internal

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Applicative
-- >>> import Data.Time
-- >>> import Data.Int
-- >>> import Data.Text (Text)
-- >>> import Data.Time (Day)
-- >>> import Data.Version

-- $examples
--
-- Booleans:
--
-- >>> toPathPiece True
-- "true"
-- >>> fromPathPiece "false" :: Maybe Bool
-- Just False
-- >>> fromPathPiece "something else" :: Maybe Bool
-- Nothing
--
-- Numbers:
--
-- >>> toPathPiece 45.2
-- "45.2"
-- >>> fromPathPiece "452" :: Maybe Int
-- Just 452
-- >>> fromPathPiece "256" :: Maybe Int8
-- Nothing
--
-- Strings:
--
-- >>> toPathPiece "hello"
-- "hello"
-- >>> fromPathPiece "world" :: Maybe String
-- Just "world"
--
-- Calendar day:
--
-- >>> toPathPiece (fromGregorian 2015 10 03)
-- "2015-10-03"
-- >>> toGregorian <$> fromPathPiece "2016-12-01"
-- Just (2016,12,1)
