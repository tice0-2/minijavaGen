{-# LANGUAGE DeriveGeneric, TemplateHaskell, BangPatterns, FlexibleInstances #-}
module Lib
    ( genProgram
    ) where

import GHC.Generics
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck
import Control.Monad
import Data.List
import AST
import Pretty
import ArbitraryAST

genProgram :: IO ()
genProgram = do
    forM_ [1..1] $ \_ -> do
        pgm <- generate (arbitrary :: Gen Program)
        putStrLn $ plain pgm