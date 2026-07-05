module Main (main) where

import System.FilePath ((</>))
import CGenerate

main :: IO ()
main = do
    let gen = do
            cGenInit
            genDefine "hello" (10 :: Int) False
            genDefine "hello1" "hello\\nhello" False

    runCGen gen $ "test" </> "file.h"