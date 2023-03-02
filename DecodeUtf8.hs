{-# LANGUAGE ForeignFunctionInterface #-}

-- | Test decoding of UTF-8
--
-- Tested in this benchmark:
--
-- * Decoding bytes using UTF-8
--
-- In some tests:
--
-- * Taking the length of the result
--
-- * Taking the init of the result
--
-- The latter are used for testing stream fusion.
--
module DecodeUtf8
    ( initEnv
    , benchmark
    ) where

import Data.ByteString.Lazy.Internal (ByteString(..))
import Test.Tasty.Bench
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

type Env = (BL.ByteString)

initEnv :: FilePath -> IO Env
initEnv fp = do
    lbs <- BL.readFile fp
    return (lbs)

benchmark :: String -> Env -> Benchmark
benchmark kind lbs =
    let decodeStream (Chunk b0 bs0) = case T.streamDecodeUtf8 b0 of
                                        T.Some t0 _ f0 -> t0 : go f0 bs0
          where go f (Chunk b bs1) = case f b of
                                       T.Some t1 _ f1 -> t1 : go f1 bs1
                go _ _ = []
        decodeStream _ = []
    in bgroup kind
        [ bench "Stream" $ nf decodeStream lbs
        ]

