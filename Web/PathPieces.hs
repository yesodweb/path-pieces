{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DefaultSignatures    #-}
module Web.PathPieces
    ( PathPiece (..)
    , PathMultiPiece (..)
    , GPathMultiPiece (..)
    , readFromPathPiece
    , showToPathPiece
    -- * Deprecated
    , toSinglePiece
    , toMultiPiece
    , fromSinglePiece
    , fromMultiPiece
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Read
import Data.Time (Day)
import Control.Exception (assert)
import Text.Read (readMaybe)
import GHC.Generics

class PathPiece s where
    fromPathPiece :: S.Text -> Maybe s
    toPathPiece :: s -> S.Text

instance PathPiece () where
    fromPathPiece t = if t == "_" then Just () else Nothing
    toPathPiece () = "_"

instance PathPiece String where
    fromPathPiece = Just . S.unpack
    toPathPiece = S.pack

instance PathPiece S.Text where
    fromPathPiece = Just
    toPathPiece = id

instance PathPiece L.Text where
    fromPathPiece = Just . L.fromChunks . return
    toPathPiece = S.concat . L.toChunks

parseIntegral :: (Integral a, Bounded a, Ord a) => S.Text -> Maybe a
parseIntegral s = n
    where
    n = case Data.Text.Read.signed Data.Text.Read.decimal s of
        Right (i, "") | i <= top && i >= bot -> Just (fromInteger i)
        _ -> Nothing
    Just witness = n
    top = toInteger (maxBound `asTypeOf` witness)
    bot = toInteger (minBound `asTypeOf` witness)

instance PathPiece Integer where
    fromPathPiece s =
        case Data.Text.Read.signed Data.Text.Read.decimal s of
            Right (i, "") -> Just i
            _ -> Nothing
    toPathPiece = S.pack . show

instance PathPiece Int where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Int8 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Int16 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Int32 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Int64 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word8 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word16 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word32 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Word64 where
    fromPathPiece = parseIntegral
    toPathPiece = S.pack . show

instance PathPiece Bool where
    fromPathPiece t =
        case filter (null . snd) $ reads $ S.unpack t of
            (a, s):_ -> assert (null s) (Just a)
            _        -> Nothing
    toPathPiece = S.pack . show

instance PathPiece Day where
    fromPathPiece t =
        case reads $ S.unpack t of
            [(a,"")] -> Just a
            _ -> Nothing
    toPathPiece = S.pack . show

instance (PathPiece a) => PathPiece (Maybe a) where
    fromPathPiece s = case S.stripPrefix "Just " s of
        Just r -> Just `fmap` fromPathPiece r
        _ -> case s of
            "Nothing" -> Just Nothing
            _ -> Nothing
    toPathPiece m = case m of
        Just s -> "Just " `S.append` toPathPiece s
        _ -> "Nothing"

-- | A typeclass for things that can be converted into multiple path pieces.
-- There are defaults methods for types that are that product of types with
-- 'PathPiece' instances. In the example below, we can get a 'PathMultiPiece'
-- instance for free:
--
-- @
-- data Foo = Foo Int Bool Text
--   deriving Generic
-- instance PathMultiPiece Foo
-- @
--
-- This behaves as you would expect:
--
-- >>> toPathMultiPiece (Foo 4 True "hello")
-- ["4","True","hello"]
--
class PathMultiPiece s where
    fromPathMultiPiece :: [S.Text] -> Maybe s
    default fromPathMultiPiece :: (Generic s, GPathMultiPiece (Rep s)) => [S.Text] -> Maybe s
    fromPathMultiPiece ts = case gfromPathMultiPiece ts of
      Nothing -> Nothing
      Just (a,xs) -> if null xs then Just (to a) else Nothing
    toPathMultiPiece :: s -> [S.Text]
    default toPathMultiPiece :: (Generic s, GPathMultiPiece (Rep s)) => s -> [S.Text]
    toPathMultiPiece = gtoPathMultiPiece . from

instance PathPiece a => PathMultiPiece [a] where
    fromPathMultiPiece = mapM fromPathPiece
    toPathMultiPiece = map toPathPiece

-- | A function for helping generate free 'PathPiece'
--   instances for enumeration data types 
--   that have derived 'Read' and 'Show' instances.
--   Intended to be used like this:
--
--   > data MyData = Foo | Bar | Baz
--   >   deriving (Read,Show)
--   > instance PathPiece MyData where
--   >   fromPathPiece = readFromPathPiece
--   >   toPathPiece = showToPathPiece
--
--  Since 0.2.1. 
readFromPathPiece :: Read s => S.Text -> Maybe s
readFromPathPiece = readMaybe . S.unpack

-- | See the documentation for 'readFromPathPiece'.
--
--  Since 0.2.1. 
showToPathPiece :: Show s => s -> S.Text
showToPathPiece = S.pack . show

class GPathMultiPiece s where
    gfromPathMultiPiece :: [S.Text] -> Maybe (s a, [S.Text])
    gtoPathMultiPiece :: s a -> [S.Text]

instance PathPiece c => GPathMultiPiece (K1 i c) where
    gtoPathMultiPiece (K1 c) = [toPathPiece c]
    gfromPathMultiPiece ts = case ts of
        [] -> Nothing
        t : ss -> fmap (\a -> (a,ss)) (fmap K1 (fromPathPiece t))

instance (GPathMultiPiece f) => GPathMultiPiece (M1 a b f) where
    gtoPathMultiPiece (M1 c) = gtoPathMultiPiece c
    gfromPathMultiPiece ts = fmap (\(a,xs) -> (M1 a, xs)) (gfromPathMultiPiece ts)

instance (GPathMultiPiece f, GPathMultiPiece g) => GPathMultiPiece ((:*:) f g) where
    gtoPathMultiPiece (f :*: g) = gtoPathMultiPiece f ++ gtoPathMultiPiece g
    gfromPathMultiPiece ts1 = do
      (f, ts2) <- gfromPathMultiPiece ts1
      (g, ts3) <- gfromPathMultiPiece ts2
      return (f :*: g,ts3)

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
