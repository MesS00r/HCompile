module Main (main) where

import System.FilePath ((</>))
import HCompile

main :: IO ()
main =
    runHCompile (do
        delFile

        send2File (
            "; ***************** OWO *****************\n\n"     ++

            "; Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n"   ++
            "; Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n"   ++
            "; Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n"   ++
            "; Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n"   ++
            "; Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo Owo\n\n"
            )

        let owoPath = "img" </> "Owo.bmp"
        owoWidth  <- getBmpWidth     owoPath
        owoHeight <- getBmpHeight    owoPath
        owoPixels <- getBmpPixelsNum owoPath
        owoBytes  <- getBmpSizeByte  owoPath

        genConst "" "OWO_WIDTH equ "  owoWidth
        genConst "" "OWO_HEIGHT equ " owoHeight
        genConst "" "OWO_PIXELS equ " owoPixels
        genConst "" "OWO_BYTES equ "  owoBytes
        send2File "\n"

        send2File "; OWO_PALETTE\n"
        genBmpPalette owoPath
                      ""
                      ("COLOR_", " equ ")
                      ("\n", "")
                      ("", "\n")
                      10
                      1

        send2File "\n"

        genBmpImage   owoPath
                      "owo: db\n\t"
                      "COLOR_"
                      (", \n\t", ", ")
                      ("", "")
                      16

    ) HCompileConf {
        filePath     = "test" </> "gen_asm" </> "owo.asm",
        constWidth   = 25
    }