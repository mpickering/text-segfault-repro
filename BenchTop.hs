{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Benchmarks
import Language.Haskell.TH
import System.Environment

main = $(runIO (withArgs []  Benchmarks.main) >> [| return () |])
