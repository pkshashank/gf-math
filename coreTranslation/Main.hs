import PGF
import ForthelDemo -- English file
import CoreDemo -- Core Gramamr file
import Prelude
import SimplifyExtended
import Data.Maybe (fromJust)
import Data.List (nub)
import UtilitiesMain
import TranslateToCore



main :: IO()
main = do
  contents <- readFile "test.txt"
  let inputLines = lines contents

  mapM_ simplifyAndPass inputLines


simplifyAndPass :: String -> IO ()
simplifyAndPass s = do
  -- Show the input
  putStrLn ("Read: " ++ s)

  -- Store extended grammar
  (extendedPGF, extendedLang) <- getPGFandLanguage "ForthelDemo.pgf"

  -- Parse the input
  let pgfTrees = parse extendedPGF extendedLang (fromJust $ readType "Toplevel") s

  -- convert the trees to haskell
  let haskellTrees = map (\x -> ForthelDemo.fg x :: ForthelDemo.GToplevel) pgfTrees

  -- simplify and remove duplicates
  let simplifiedHTrees = nub $ map (stackAssume .
                          splitStmAnds .
                          deLatex .
                          letToAssume .
                          changeUniversals .
                          ifThenToAssume ) haskellTrees

  mapM_ convertToCoreAndPass simplifiedHTrees


convertToCoreAndPass :: ForthelDemo.GToplevel -> IO ()
convertToCoreAndPass htree = do
  -- Output after simplification
  (extendedPGF, extendedLang) <- getPGFandLanguage "ForthelDemo.pgf"
  putStrLn $ "Step 1: " ++ linearize extendedPGF extendedLang (ForthelDemo.gf htree)

  -- Store core grammar
  (corePGF,coreLang) <- getPGFandLanguage "CoreDemo.pgf"

  -- Convert to core
  let coreTree = translateToplevel htree
  
  -- Output after conversion
  putStrLn $ "Step 2: " ++ linearize corePGF coreLang (CoreDemo.gf coreTree) ++ "\n"
  return ()
