module Main (main) where

import System.FilePath ((</>))
import HCompile

main :: IO ()
main =
    runHCompile (do
        hCompileBegin ("// ------------- TITLE -------------\n\n" ++
                      "#include <stdint.h>\n" ++
                      "#include <math.h>\n\n")

        send2File "// COMMENT :)\n"

        genDefine    "HELLO" (((10 + 10) * 5) :: Int)
        genDefine    "HELLO1" "hello\\nhello"
        genDefineRaw "HELLO2" "HELLO"

        send2File "\n"

        genDefineMacro "MACRO(a, b) " "((a) + (b))"

        send2File "\n"

        genCType         "static const char* name[] = " ["a", "b", "c", "d", "e"] ", " 2
        genTableExp      "static const char* name2" (\f -> show f) (256 :: Int) 16
        genTableExpFloat "static const char* name3" (\f -> show (f / 2)) (0 :: Float, 256) 0.1 16

        genTableExpRaw      "static const uint16_t name4" (\f -> show f) (256 :: Int) 16
        genTableExpFloatRaw "static const float name5" (\f -> show (f / 2)) (0 :: Float, 256) 0.1 16

        hCompileEnd
    ) $ "test" </> "file.h"