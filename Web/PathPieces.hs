{-# LANGUAGE FlexibleInstances, TypeSynonymInstances  #-}
module Web.PathPieces
    ( PathPiece (..)
    , PathMultiPiece (..)
    -- * Deprecated
    , toSinglePiece
    , toMultiPiece
    , fromSinglePiece
    , fromMultiPiece
    ) where

import Data.Int (Int64)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Read
import Data.Time (Day)

class PathPiece s where
    fromPathPiece :: S.Text -> Maybe s
    toPathPiece :: s -> S.Text

instance PathPiece String where
    fromPathPiece = Just . S.unpack
    toPathPiece = S.pack

instance PathPiece S.Text where
    fromPathPiece = Just
    toPathPiece = id

instance PathPiece L.Text where
    fromPathPiece = Just . L.fromChunks . return
    toPathPiece = S.concat . L.toChunks

instance PathPiece Integer where
    fromPathPiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toPathPiece = S.pack . show

instance PathPiece Int where
    fromPathPiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toPathPiece = S.pack . show

instance PathPiece Int64 where
    fromPathPiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toPathPiece = S.pack . show

instance PathPiece Day where
    fromPathPiece t =
        case reads $ S.unpack t of
            [(a,"")] -> Just a
            _ -> Nothing
    toPathPiece = S.pack . show

class PathMultiPiece s where
    fromPathMultiPiece :: [S.Text] -> Maybe s
    toPathMultiPiece :: s -> [S.Text]

instance PathPiece p => PathMultiPiece [p] where
    fromPathMultiPiece = sequence . map fromPathPiece   -- (in the Maybe monad)
    toPathMultiPiece = map toPathPiece

{-# DEPRECATED toSinglePiece "Use toPathPiece instead of toSinglePiece" #-}
toSinglePiece :: PathPiece p => p -> S.Text
toSinglePiece = toPathPiece

{-# DEPRECATED fromSinglePiece "Use fromPathPiece instead of fromSinglePiece" #-}
fromSinglePiece :: PathPiece p => S.Text -> Maybe p
fromSinglePiece = fromPathPiece

{-# DEPRECATED toMultiPiece "Use toPathMultiPiece instead of toMultiPiece" #-}
toMultiPiece :: PathMultiPiece ps => ps -> [S.Text]
toMultiPiece = toPathMultiPiece

{-# DEPRECATED fromMultiPiece "Use fromPathMultiPiece instead of fromMultiPiece" #-}
fromMultiPiece :: PathMultiPiece ps => [S.Text] -> Maybe ps
fromMultiPiece = fromPathMultiPiece
