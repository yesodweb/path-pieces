-- | Defines two typeclasses used for converting Haskell data types to and from
--   route pieces:
--
--   * 'PathPiece'
--   * 'PathMultiPiece'
--
-- They are used in <https://www.yesodweb.com/ Yesod> to automatically marshall
-- data in the request path.
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}
module Web.PathPieces
    ( PathPiece (..)
    , PathMultiPiece (..)
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

-- | Instances of the 'PathPiece' typeclass can be converted to and from "path
--   pieces" of URLs.
--   <https://www.yesodweb.com/book/routing-and-handlers#routing-and-handlers_types_of_pieces In Yesod this typeclass is used to convert to and from route pieces>.
--
--   In the URL @"https://example.com/path/True/7"@ there are three path
--   pieces: @"path"@, @\"True\"@, @"7"@.  These can be converted respectively into
--   the 'Data.Text.Text', 'Bool' and 'Int' types like so:
--
--   > > fromPathPiece "path" :: Maybe Text
--   > Just "path"
--   > > fromPathPiece "True" :: Maybe Bool
--   > Just True
--   > > fromPathPiece "7" :: Maybe Int
--   > Just 7
--
--   The return type of 'fromPathPiece' is a 'Maybe' to account for that the
--   conversion may fail:
--
--   > > fromPathPiece "seven" :: Maybe Int
--   > Nothing
--
-- | The 'toPathPiece' function produces URL path pieces can can be used to
--   build URLs:
--
--   > > toPathPiece True
--   > "True"
--   > > intercalate "/" [toPathPiece ("path" :: Text), toPathPiece True, toPathPiece (7 :: Int)]
--   > "path/True/7"
--
--   Here is an example instance of a @Natural@ type encoding that the
--   'PathPiece' must always be a non-negative integer.
--
--   > newtype Natural = Natural Int
--   > instance PathPiece Natural where
--   >     toPathPiece (Natural i) = T.pack $ show i
--   >     fromPathPiece s =
--   >         case reads $ T.unpack s of
--   >             (i, ""):_
--   >                 | i < 1 -> Nothing
--   >                 | otherwise -> Just $ Natural i
--   >             [] -> Nothing
--
--   In Yesod, after declaring the above instance, @Natural@ will be able to
--   appear in routes, e.g.:
--
--   > /fib/#Natural
--   > /add/#Natural/#Natural
class PathPiece s where
    fromPathPiece :: S.Text -> Maybe s
    toPathPiece :: s -> S.Text

-- | Represents @()@ as @"_"@.
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

-- | Instances of the 'PathMultiPiece' typeclass can be converted to and from
--   several "path pieces" of URLs.
--
--   > > fromPathMultiPiece ["True","False","True"] :: Maybe [Bool]
--   > Just [True,False,True]
--   > > toPathMultiPiece [True,False,True]
--   > ["True","False","True"]
--   > > toPathMultiPiece [1, 2, 3 :: Int]
--   > ["1","2","3"]
--
--   In Yesod, instances of 'PathMultiPiece' can be marshalized to and from the
--   rest of the values in a URL path:
--
--   > /bools/*[Bool]
--   > /integers/*[Int]
class PathMultiPiece s where
    fromPathMultiPiece :: [S.Text] -> Maybe s
    toPathMultiPiece :: s -> [S.Text]

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
