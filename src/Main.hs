module Main (main) where

import System.FilePath ((</>))
import HCompile

main :: IO ()
main =
    runHCompile (do
        delFile
        
        fileName <- getFileName
        send2File (
                       "#ifndef " ++ map cleanName fileName ++ "\n"   ++
                       "#define " ++ map cleanName fileName ++ "\n\n" ++

                       "// ------------- TITLE -------------\n\n"     ++
                       
                       "#include <stdint.h>\n"                        ++
                       "#include <math.h>\n\n"
                    )

        send2File "// COMMENT :)\n"

        genConst    "#define" "HELLO" (((10 + 10) * 5) :: Int)
        genConst    "#define" "HELLO1" "hello\\nhello"
        genConstRaw "#define" "HELLO2" "HELLO"

        send2File "\n"

        genMacro "#define" "MACRO(a, b)" "((a) + (b))"

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

        let bmpFilePath = ("img" </> "img8x8GIMPIndexed9.bmp")

        genBmpPalette bmpFilePath 
                      "enum NAME " "COLOR_" 
                      ", " ("{\n\t", "\n};\n") 1

        genBmpImage   bmpFilePath 
                      "static const uint32_t name6[] = "
                      "COLOR_" ", " ("{\n\t", "\n};\n") 16

        send2File ("\n#endif // " ++ map cleanName fileName)
    )
    HCompileConf {
        filePath     = "test" </> "file.h", 
        constWidth   = 7,
        paletteWidth = 10
    }