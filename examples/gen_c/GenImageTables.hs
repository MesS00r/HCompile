module Main (main) where

import System.FilePath ((</>))
import HCompile

main :: IO ()
main =
    runHCompile (do
        delFile

        fileName <- getFileName
        send2File (
            "#ifndef " ++ map cleanName fileName ++ "\n"              ++
            "#define " ++ map cleanName fileName ++ "\n\n"            ++

            "// ***************** IMAGE TABLES *****************\n\n" ++

            "// These are image tables. The end.\n\n"                 ++

            "#include <stdint.h>\n\n"
            )

-- * IMAGE 1
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

        send2File ("// * -------------------------------------------------------------------------------\n" ++
                  "// * IMAGE 1 (IMG_RGBY_8X8)\n"                                                           ++
                  "// * -------------------------------------------------------------------------------\n\n"
                  )

        let img1Path = "img" </> "imgRGBY(8x8)x4.bmp"
        img1Width  <- getBmpWidth     img1Path
        img1Height <- getBmpHeight    img1Path
        img1Pixels <- getBmpPixelsNum img1Path
        img1Bytes  <- getBmpSizeByte  img1Path

        genConst "#define" "IMG_RGBY_8x8_WIDTH"  img1Width
        genConst "#define" "IMG_RGBY_8x8_HEIGHT" img1Height
        genConst "#define" "IMG_RGBY_8x8_PIXELS" img1Pixels
        genConst "#define" "IMG_RGBY_8x8_BYTES"  img1Bytes
        send2File "\n"

        genBmpPalette img1Path
                      "enum IMG_RGBY_8x8_PALETTE "
                      "COLOR_" ", " ("{\n\t", "\n};\n") 1

        genBmpImage   img1Path
                      "static const uint32_t imgRgby8x8[] = "
                      "COLOR_" ", " ("{\n\t", "\n};\n") 16

-- * IMAGE 2
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

        send2File ("\n// * -------------------------------------------------------------------------------\n" ++
                  "// * IMAGE 2 (MUSHROOM)\n"                                                                 ++
                  "// * -------------------------------------------------------------------------------\n\n"
                  )

        let img2Path = "img" </> "mushroom.bmp"
        img2Width  <- getBmpWidth     img2Path
        img2Height <- getBmpHeight    img2Path
        img2Pixels <- getBmpPixelsNum img2Path
        img2Bytes  <- getBmpSizeByte  img2Path

        genConst "#define" "MUSHROOM_WIDTH"  img2Width
        genConst "#define" "MUSHROOM_HEIGHT" img2Height
        genConst "#define" "MUSHROOM_PIXELS" img2Pixels
        genConst "#define" "MUSHROOM_BYTES"  img2Bytes
        send2File "\n"

        let customPalette = [1, 2, 3, 4, 5]
        genTypeRaw    "enum MUSHROOM_PALETTE "
                      (map (\(i, f)-> padName 10 ("COLOR1_" ++ show i) ++ " = " ++ show f)
                      (zip[0 :: Int ..] customPalette))
                      ", " ("{\n\t", "\n};\n") 1

        genBmpImage   img2Path
                      "static const uint8_t mushroom[] = "
                      "COLOR1_" ", " ("{\n\t", "\n};\n") 16

        send2File ("\n#endif // " ++ map cleanName fileName)

    ) HCompileConf {
        filePath     = "test" </> "gen_c" </> "image_tables.h",
        constWidth   = 25,
        paletteWidth = 10
    }