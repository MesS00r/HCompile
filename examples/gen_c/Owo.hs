module Main (main) where

import System.FilePath ((</>))
import HCompile

main :: IO ()
main =
    runHCompile (do
        delFile

        fileName <- getFileName
        send2File (
            "#ifndef " ++ map cleanName fileName ++ "\n"         ++
            "#define " ++ map cleanName fileName ++ "\n\n"       ++

            "// ***************** OWO *****************\n\n"     ++

            "// Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n"   ++
            "// Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n"   ++
            "// Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n"   ++
            "// Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n"   ++
            "// Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n\n" ++

            "#include <stdint.h>\n\n"
            )

        let owoPath = "img" </> "Owo.bmp"
        owoWidth  <- getBmpWidth     owoPath
        owoHeight <- getBmpHeight    owoPath
        owoPixels <- getBmpPixelsNum owoPath
        owoBytes  <- getBmpSizeByte  owoPath

        genConst "#define" "OWO_WIDTH"  owoWidth
        genConst "#define" "OWO_HEIGHT" owoHeight
        genConst "#define" "OWO_PIXELS" owoPixels
        genConst "#define" "OWO_BYTES"  owoBytes
        send2File "\n"

        genBmpPalette owoPath
                      "enum OWO_PALETTE "
                      "COLOR_" ", " ("{\n\t", "\n};\n") 1

        genBmpImage   owoPath
                      "static const uint32_t owo[] = "
                      "COLOR_" ", " ("{\n\t", "\n};\n") 16

        send2File ("\n#endif // " ++ map cleanName fileName)

    ) HCompileConf {
        filePath     = "test" </> "gen_c" </> "owo.h",
        constWidth   = 25,
        paletteWidth = 10
    }