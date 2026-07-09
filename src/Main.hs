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

        genMacro "MACRO(a, b)" "((a) + (b))"

        send2File "\n"

        genType             "static const char* name[] = "  
                            ["a", "b", "c", "d", "e"] 
                            ", " ("{\n\t", "\n};\n") 2

        genTableFoo         "static const char* name2[] = " 
                            (\f -> show f) 
                            (255 :: Int) 
                            ", " ("{\n\t", "\n};\n") 16
        
        genTableFooFloat    "static const char* name3[] = "
                            (\f -> show (sin f)) 
                            (0 :: Float, 256)
                            0.5 ", " ("{\n\t", "\n};\n") 16

        genTableFooRaw      "static const uint16_t name4[] = "
                            (\f -> show f) 
                            (256 :: Int)
                            ", " ("{\n\t", "\n};\n") 16

        genTableFooFloatRaw "static const float name5[] = "
                            (\f -> show (sin f))
                            (0 :: Float, 256) 
                            0.5 ", " ("{\n\t", "\n};\n") 16

        hCompileEnd
    )
    HCompileConf {
        filePath = "test" </> "file.h", 
        defineWidth = 7
    }