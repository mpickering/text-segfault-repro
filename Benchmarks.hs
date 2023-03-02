{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Benchmarks
    ( main
    ) where

import Test.Tasty.Bench (defaultMain, bgroup, env)
import qualified DecodeUtf8 as DecodeUtf8

main :: IO ()
main = do
    defaultMain
        [ bgroup "DecodeUtf8"[
              env (DecodeUtf8.initEnv ("japanese.txt")) (DecodeUtf8.benchmark "japanese")
            ]
        ]
