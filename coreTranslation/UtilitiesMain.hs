module UtilitiesMain where

import PGF

getPGFandLanguage :: String -> IO (PGF, Language)
getPGFandLanguage fileName = do
  pgf <- readPGF fileName
  return (pgf, head $ languages pgf)
