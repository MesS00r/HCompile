module Main (main) where

import System.FilePath ((</>))
import HCompile

main :: IO ()
main =
    runHCompile (do
        hCompileBegin "// ------------- TITLE -------------\n\n"

        send2File "// COMMENT :)\n"

        genDefine    "HELLO" (((10 + 10) * 5) :: Int)
        genDefine    "HELLO1" "hello\\nhello"
        genDefineRaw "HELLO2" "HELLO"

        send2File "\n"

        genDefineMacro "MACRO(a, b) " "((a) + (b))"

        send2File "\n"

        genCType         "static uint8_t name[] = " [1 :: Int, 2, 3, 4, 5] ", " 2
        genTableExp      "static int16_t name2" (\f -> f - 2) (256 :: Int) 16
        genTableExpFloat "static float name3" (\f -> sin f) (0 :: Float, 256) 0.1 16

        hCompileEnd
    ) $ "test" </> "file.h"