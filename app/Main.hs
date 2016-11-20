{- -*- mode: haskell; eval: (turn-on-haskell-unicode-input-method) -*- -}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude.Unicode
import Control.Monad.Unicode

import Control.Monad (filterM, liftM, forM_)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.IO (readFile)
import System.Directory (doesFileExist, renameFile)
import System.Exit (ExitCode(..))
import System.File.Tree (getDirectory, flatten)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (createProcess, waitForProcess, proc, cwd, std_out, delegate_ctlc, StdStream(CreatePipe))
import Text.LaTeX.Base.Render (renderFile)

import Lib

main ∷ IO ()
main =
  withSystemTempDirectory "qrar" $ \tmpDir → do
    generateTexFile "." (tmpDir </> "qrar.tex")
    (_, Just hout, _, ph) ← createProcess (proc "pdflatex" ["qrar.tex"]){ cwd = Just tmpDir
                                                                        , std_out = CreatePipe
                                                                        , delegate_ctlc = True }
    out ← B.hGetContents hout
    ec ← waitForProcess ph
    case ec of
      ExitFailure r → do
        B.putStrLn out
        forM_ ["qrar.tex", "qrar.log"] $ \f →
          renameFile (tmpDir </> f) f
        ioError ∘ userError $ "pdflatex"
      ExitSuccess → renameFile (tmpDir </> "qrar.pdf") "qrar.pdf"

generateTexFile ∷ FilePath → FilePath → IO ()
generateTexFile rootDir texFileName =
      return rootDir
  ≫= getDirectory
  ≫= return ∘ flatten
  ≫= filterM doesFileExist
  ≫= mapM (\x → liftM ((,) x) (Data.Text.IO.readFile x))
  ≫= renderFile texFileName ∘ filelistToLatex
