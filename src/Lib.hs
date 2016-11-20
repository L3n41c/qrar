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

filelistToLatex ∷ [(FilePath, Text)] → LaTeX
filelistToLatex filelist =
  thePreamble ⊕ document (theBody filelist)

thePreamble ∷ LaTeX
thePreamble =
    documentclass [Paper A4] article
  ⊕ useencoding [T1]
  ⊕ usepackage [utf8] inputenc
  ⊕ usepackage [] qrcode

theBody ∷ [(FilePath, Text)] → LaTeX
theBody filelist =
  mconcat $ map fileToLatex filelist

fileToLatex ∷ (FilePath, Text) → LaTeX
fileToLatex (filepath, filecontent) =
    section (texttt $ fromString filepath)
  ⊕ qr CodeOptions{ includePadding = True
                  , link = False
                  , errorLevel = High} filecontent
