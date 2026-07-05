module Main (main) where

import System.FilePath ((</>))
import HCompile

main :: IO ()
main = do
    let gen = do
            hCompileBegin "// ------------- TITLE -------------\n\n"

            send2File "// COMMENT :)\n"

            genDefine    "HELLO" (((10 + 10) * 5) :: Int)
            genDefine    "HELLO1" "hello\\nhello"
            genDefineRaw "HELLO2" "HELLO"

            send2File "\n"

            genDefineMacro "MACRO(a, b) " "((a) + (b))"

            hCompileEnd

    runCGen gen $ "test" </> "file.h"   