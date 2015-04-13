{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import Data.Char    ( toUpper )
import Data.Functor ( (<$>) )

import Data.Text ( Text )
import qualified Data.Text as T

import Control.Monad  ( replicateM, unless )

import System.Directory   ( copyFile )
import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure )
import System.FilePath    ( (</>) )

import System.IO
  ( IOMode(ReadMode)
  , hGetChar
  , hPrint
  , hPutStr
  , hPutStrLn
  , stderr
  , withBinaryFile
  )

import Text.PDF.Info
  ( PDFInfo
  , pdfInfo
  , pdfInfoAuthor
  , pdfInfoCreationDate
  , pdfInfoTitle
  )

------------------------------------------------------------------------------
-- Utilities from the Agda library

ifM ∷ Monad m ⇒ m Bool → m a → m a → m a
ifM c m m' = do
  b ← c
  if b then m else m'

-- | @unless_@ is just @Control.Monad.unless@ with a more general type.
unless_ ∷ Monad m ⇒ Bool → m a → m ()
unless_ b m = unless b $ m >> return ()

unlessM ∷ Monad m ⇒ m Bool → m a → m ()
unlessM c m = c >>= (`unless_` m)

------------------------------------------------------------------------------
-- Utilities

failureMsg ∷ String → IO ()
failureMsg err = getProgName >>= \p → hPutStrLn stderr $ p ++ ": " ++ err

replace ∷ [(Text,Text)] → Text → Text
replace []       ys = ys
replace (x : xs) ys = replace xs $ uncurry T.replace x ys

onlyFirstLetterUpperCase ∷ Text → Text
onlyFirstLetterUpperCase xs =
  case T.uncons xs of
    Nothing        → T.empty
    Just (x', xs') → T.cons (toUpper x') (T.toLower xs')

------------------------------------------------------------------------------

defaultValue ∷ Text
defaultValue = "XX"

authorsReplacements ∷ [(Text, Text)]
authorsReplacements =
  [ (", ",       ",")
  , (" and",     ",")
  , ("á",        "a")
  , ("é",        "e")
  , ("í",        "i")
  , ("ó",        "o")
  , ("ú",        "u")
  , ("ö",        "o")
  , ("Á",        "A")
  , ("É",        "E")
  , ("Í",        "I")
  , ("Ó",        "O")
  , ("Ú",        "U")
  , ("&#246;",   "o")  -- Symbol ö.
  , ("&Auml;",   "A")  -- Symbol Ä.
  , ("&Uuml;",   "U")  -- Symbol Ü.
  , ("Mcbride",  "McBride")
  ]

getAuthors ∷ Text → Text
getAuthors xs =
  if T.null xs
  then defaultValue
  else
    ( replace authorsReplacements
      . T.intercalate (T.singleton '-')
      . map (onlyFirstLetterUpperCase . T.reverse . T.takeWhile (' ' /=) . T.reverse)
      . T.split (',' ==)
    ) xs

getYear ∷ Text → Text
getYear xs = if T.null xs then defaultValue else T.take 4 xs

titleReplacements ∷ [(Text,Text)]
titleReplacements =
  [ ("<Emphasis Type=\"Italic\">0 </Emphasis>", "0")
  , (":",        ".")
  , ("/",        "-")
  , ("(",        "")
  , (")",        "")
  , (" ",        "-")
  , (",",        "")
  , ("'",        "")
  , ("[",        "")
  , ("]",        "")
  , ("?",        "")
  , ("!",        "")
  , ("ü",        "u")
  , ("&#955;",   "lambda") -- Symbol λ.
  , ("&#x00DC;", "U")      -- Symbol Ü.
  , ("&#x00F6;", "o")      -- Symbol ö.
  , ("&#x00FC;", "u")      -- Symbol ü.
  , ("&#x03BB;", "lambda") -- Symbol λ.
  , ("&#x2014;", ".")      -- em dash character '—'.
  , ("algol",    "Algol")
  , ("church",   "Church")
  , ("coq",      "Coq")
  , ("goles",    "Goles")
  , ("kleene",   "Kleene")
  , ("lesbegue", "Lesbegue")
  , ("twelf",    "Twelf")
  ]

getTitle ∷ Text → Text
getTitle xs =
  if T.null xs
  then defaultValue
  else
    ( onlyFirstLetterUpperCase
      . replace titleReplacements
    ) xs

generateFileName ∷ PDFInfo → FilePath
generateFileName info = fileName
  where
  authors ∷ Text
  authors = case pdfInfoAuthor info of
              Nothing → defaultValue
              Just a  → getAuthors a

  year ∷ Text
  year = case pdfInfoCreationDate info of
           Nothing → defaultValue
           Just y  → getYear $ T.pack $ show y

  title ∷ Text
  title = case pdfInfoTitle info of
            Nothing → defaultValue
            Just t  → getTitle t

  fileName ∷ FilePath
  fileName = T.unpack $ foldl1 T.append
               [ authors
               , T.singleton '-'
               , year
               , T.singleton '.'
               , title
               , ".pdf"
               ]

outputDir ∷ FilePath
outputDir = "/tmp/"

createFile ∷ FilePath → FilePath → IO ()
createFile f newF =
  copyFile f outputPath >> putStrLn ("Created " ++ outputPath)
  where
  outputPath ∷ FilePath
  outputPath = outputDir </> newF

isPDF ∷ FilePath → IO Bool
isPDF f =
  ifM ((==) "%PDF" <$> withBinaryFile f ReadMode (replicateM 4 . hGetChar))
      (return True)
      (return False)

------------------------------------------------------------------------------

main ∷ IO ()
main = do
  f ← fmap head getArgs

  unlessM (isPDF f) $ do
    failureMsg $ f ++ " is not a PDF file"
    exitFailure

  info ← pdfInfo f
  case info of
    Right i   → createFile f $ generateFileName i
    Left  err → do
      hPutStr stderr "pdfInfo exception: "
      hPrint stderr err
      exitFailure
