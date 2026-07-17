module Main (main) where

import System.FilePath ((</>))
import HCompile

main :: IO ()
main =
    runHCompile (do
        delFile

        send2File (
            "; ***************** IMAGE TABLES *****************\n\n"     ++

            "; These are image tables. The end.\n\n"
            )

-- * IMAGE 1
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

        send2File ("; * -------------------------------------------------------------------------------\n" ++
                  "; * IMAGE 1 (IMG_RGBY_8X8)\n"                                                           ++
                  "; * -------------------------------------------------------------------------------\n\n"
                  )

        let img1Path = "img" </> "imgRGBY(8x8)x4.bmp"
        img1Width  <- getBmpWidth     img1Path
        img1Height <- getBmpHeight    img1Path
        img1Pixels <- getBmpPixelsNum img1Path
        img1Bytes  <- getBmpSizeByte  img1Path

        genConst "" "IMG_RGBY_8x8_WIDTH equ "  img1Width
        genConst "" "IMG_RGBY_8x8_HEIGHT equ " img1Height
        genConst "" "IMG_RGBY_8x8_PIXELS equ " img1Pixels
        genConst "" "IMG_RGBY_8x8_BYTES equ "  img1Bytes
        send2File "\n"

        genBmpPalette img1Path
                      ""
                      ("COLOR_", " equ ")
                      ("\n", "")
                      ("", "\n")
                      1

        genBmpImage   img1Path
                      "img_rgby_8x8: dd\n\t"
                      "COLOR_"
                      (",\n\t", ", ")
                      ("", "\n")
                      16
        send2File "\n"

-- * IMAGE 2
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

        send2File ("; * -------------------------------------------------------------------------------\n" ++
                  "; * IMAGE 2 (MUSHROOM)\n"                                                                 ++
                  "; * -------------------------------------------------------------------------------\n\n"
                  )

        let img2Path = "img" </> "mushroom.bmp"
        img2Width  <- getBmpWidth     img2Path
        img2Height <- getBmpHeight    img2Path
        img2Pixels <- getBmpPixelsNum img2Path
        img2Bytes  <- getBmpSizeByte  img2Path

        genConst "" "MUSHROOM_WIDTH equ "  img2Width
        genConst "" "MUSHROOM_HEIGHT equ " img2Height
        genConst "" "MUSHROOM_PIXELS equ " img2Pixels
        genConst "" "MUSHROOM_BYTES equ "  img2Bytes
        send2File "\n"

        let customPalette = [1, 2, 3, 4, 5]
        genTypeRaw    ""
                      (map (\(i, f)-> padName 10 ("COLOR1_" ++ show i ++ " equ ") ++ show f)
                      (zip[0 :: Int ..] customPalette))
                      ("\n", "")
                      ("", "\n")
                      1

        genBmpImage   img2Path
                      "mushroom: db\n\t"
                      "COLOR1_"
                      (",\n\t", ", ")
                      ("", "\n")
                      16

    ) HCompileConf {
        filePath     = "test" </> "gen_asm" </> "image_tables.asm",
        constWidth   = 25,
        paletteWidth = 10
    }