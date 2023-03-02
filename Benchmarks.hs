{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Benchmarks
    ( main
    ) where

import Test.Tasty.Bench (defaultMain, bgroup, env)
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

decodeStream (Chunk b0 bs0) = case T.streamDecodeUtf8 b0 of
                                T.Some t0 _ f0 -> t0 : go f0 bs0
  where go f (Chunk b bs1) = case f b of
                               T.Some t1 _ f1 -> t1 : go f1 bs1
        go _ _ = []
decodeStream _ = []

benchmark :: String -> Env -> Benchmark
benchmark kind lbs =
    bgroup kind
        [ bench "Stream" $ nf decodeStream lbs
        ]

main :: IO ()
main = do
    defaultMain
        [ bgroup "DecodeUtf8"[
              env (initEnv ("japanese.txt")) (benchmark "japanese")
            ]
        ]
