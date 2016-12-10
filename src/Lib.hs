{- -*- mode: haskell; eval: (turn-on-haskell-unicode-input-method) -*- -}
{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Lib
    ( filelistToLatex
    ) where

import Prelude.Unicode
import Data.Monoid.Unicode

import Text.LaTeX
import Text.LaTeX.Packages.Fontenc
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.QRCode

filelistToLatex ∷ (Monad m) ⇒ [(FilePath, Text)] → LaTeXT_ m
filelistToLatex filelist = do
  thePreamble
  document $ theBody filelist

thePreamble ∷ (Monad m) ⇒ LaTeXT_ m
thePreamble = do
  documentclass [Paper A4] article
  useencoding [T1]
  usepackage [utf8] inputenc
  usepackage [] qrcode

theBody ∷ (Monad m) ⇒ [(FilePath, Text)] → LaTeXT_ m
theBody filelist =
  mconcat $ map fileToLatex filelist

fileToLatex ∷ (Monad m) ⇒ (FilePath, Text) → LaTeXT_ m
fileToLatex (filepath, filecontent) = do
  section (texttt $ fromString filepath)
  qr CodeOptions{ includePadding = True
                , link = False
                , errorLevel = High} filecontent
