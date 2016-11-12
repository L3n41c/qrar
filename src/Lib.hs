{- -*- mode: haskell; eval: (turn-on-haskell-unicode-input-method) -*- -}
{-# LANGUAGE UnicodeSyntax #-}

module Lib
    ( someFunc
    ) where

import Prelude.Unicode

someFunc ∷ IO ()
someFunc = putStrLn "someFunc"
