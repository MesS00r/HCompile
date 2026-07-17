module Main (main) where

import System.FilePath ((</>))
import HCompile

main :: IO ()
main =
    runHCompile (do
        delFile

        fileName <- getFileName
        send2File (
            "; ***************** MATH TABLES *****************\n\n"     ++

            "; Math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math\n"    ++
            "; math and math and math math and math math and math.\n\n"
            )

        genConst    "" "PI equ "  (3.14 :: Float)
        genConst    "" "E equ "   (2.71 :: Float)
        send2File "\n"

        genMacro "%define " "MAX(x, y) " "((x) > (y) ? (x) : (y))"
        genMacro "%define " "MIN(x, y) " "((x) < (y) ? (y) : (x))\n"

        genMacro "%define " "SQUARE(x) " "((x) * (x))"
        genMacro "%define " "ABS(x) "    "((x) < 0 ? -(x) : (x))\n"

        genTableFoo         "numsx2: dw\n\t"
                            (\f -> f * 2)
                            (0, 256 :: Int) 1
                            (", \n\t", ", ")
                            ("", "\n")
                            16
        send2File "\n"

        genTableFooFloat    "sin_table: dd\n\t"
                            (\f -> sin f)
                            (0, 256 :: Float)
                            1
                            (", \n\t", ", ")
                            ("", "\n")
                            8
        send2File "\n"

    ) HCompileConf {
        filePath     = "test" </> "gen_asm" </> "math_tables.asm",
        constWidth   = 25,
        paletteWidth = 10
    }