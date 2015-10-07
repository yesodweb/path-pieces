-- |
-- Convert Haskell values to and from route pieces.
module Web.PathPieces (
  PathPiece (..),
  PathMultiPiece (..),

  readFromPathPiece,
  showToPathPiece,

  -- * Deprecated
  toSinglePiece,
  toMultiPiece,
  fromSinglePiece,
  fromMultiPiece,
) where

import Web.PathPieces.Internal

