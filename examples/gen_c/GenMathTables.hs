module Main (main) where

import System.Random   (randomRIO)
import System.FilePath ((</>))
import HCompile

main :: IO ()
main =
    runHCompile (do
        delFile

        fileName <- getFileName
        send2File (
            "#ifndef " ++ map cleanName fileName ++ "\n"                 ++
            "#define " ++ map cleanName fileName ++ "\n\n"               ++

            "// ***************** MATH TABLES *****************\n\n"     ++

            "// Math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math\n"    ++
            "// math and math and math math and math math and math.\n\n" ++

            "#include <stdint.h>\n\n"
            )

        genConst    "#define" "PI"  (3.14 :: Float)
        genConstRaw "#define" "TAU" "(PI * 2)"
        genConst    "#define" "E"   (2.71 :: Float)
        send2File "\n"

        randomNum <- randomRIO (1, 256 :: Int)
        genConst "#define" "RANDOM_NUM" randomNum

        genMacro "#define" "MAX(x, y)" "((x) > (y) ? (x) : (y))"
        genMacro "#define" "MIN(x, y)" "((x) < (y) ? (y) : (x))\n"

        genMacro "#define" "SQUARE(x)" "((x) * (x))"
        genMacro "#define" "ABS(x)"    "((x) < 0 ? -(x) : (x))\n"

        genTableFoo         "static const uint16_t numsx2[] = "
                            (\f -> f * 2) (0, 256 :: Int) 1
                            ", " ("{\n\t", "\n};\n") 16
        send2File "\n"

        genTableFooFloat    "static const float sin_table[] = "
                            (\f -> sin f) (0, 256 :: Float) 1
                            ", " ("{\n\t", "\n};\n") 8
        send2File "\n"

        genTableFooRaw      "static const uint16_t random_nums[] = "
                            (\f -> "RANDOM_NUM * " ++ show f) (1, 256 :: Int) 1
                            ", " ("{\n\t", "\n};\n") 8
        send2File "\n"

        genTableFooFloatRaw "static const float pi_nums[] = "
                            (\f -> "PI * " ++ show f) (1, 256 :: Float) 1
                            ", " ("{\n\t", "\n};\n") 8

        send2File ("\n#endif // " ++ map cleanName fileName)

    ) HCompileConf {
        filePath     = "test" </> "gen_c" </> "math_tables.h",
        constWidth   = 25,
        paletteWidth = 10
    }