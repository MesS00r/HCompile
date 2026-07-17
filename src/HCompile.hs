{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module HCompile (
    HCompileConf(..),
    HCompile,
    Val2String(..),
    Color24,
    delFile,
    send2File,
    cleanName,
    getFileName,
    padName,
    genConst,
    genConstRaw,
    genMacro,
    genType,
    genTypeRaw,
    genTableFoo,
    genTableFooRaw,
    genTableFooFloat,
    genTableFooFloatRaw,
    getBmpWidth,
    getBmpHeight,
    getBmpPixelsNum,
    getBmpSizeByte,
    getBmpPalette,
    getBmpImage,
    genBmpPalette,
    genBmpImage,
    runHCompile
) where

import Control.Monad.Reader
import System.Directory     (removeFile, doesFileExist, getFileSize)
import Data.Char            (toUpper, isAlphaNum)
import Data.List            (intercalate)
import Control.Monad        (when)

import Codec.Picture
import Codec.Picture.Types            
import qualified Data.ByteString      as B
import qualified Data.Vector.Storable as V
import Codec.Picture.Bitmap           (decodeBitmapWithPaletteAndMetadata)
import Data.Word                      (Word8, Word32)
import Numeric                        (showHex)
import Foreign.Storable

-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
-- * HCOMPILE TYPE
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

data HCompileConf = HCompileConf {
    constWidth   :: Int,
    paletteWidth :: Int,
    filePath     :: FilePath
    }
type HCompile h = ReaderT HCompileConf IO h

-- * HCOMPILE FOOS
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

delFile :: HCompile ()
delFile = do
    fileName <- asks filePath

    liftIO $ doesFileExist fileName >>=
        flip when (removeFile fileName)

send2File :: String -> HCompile ()
send2File str = do
    fileName <- asks filePath

    liftIO $ appendFile fileName str

cleanName :: Char -> Char
cleanName ch
    | isAlphaNum ch = toUpper ch
    | otherwise     = '_'

getFileName :: HCompile String
getFileName = asks filePath

padName :: Int -> String -> String 
padName spaces name
    | spaceNeeded > 0 = name ++ replicate spaceNeeded ' '
    | otherwise       = name
    where
        spaceNeeded = spaces - length name

-- * RAW TYPE & VAL2STRING CLASS
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

data IsRaw = Raw | NotRaw

class Val2String v where
    _toString :: v -> String
    _toRaw    :: v -> String

instance {-# OVERLAPPABLE #-} (Num v, Show v) => Val2String v where
    _toString = show
    _toRaw    = show

instance Val2String String where
    _toString str = "\"" ++ str ++ "\""
    _toRaw str    = str

instance Val2String Char where
    _toString ch = "\'" ++ [ch] ++ "\'"
    _toRaw ch    = [ch]

instance Val2String Bool where
    _toString bool = if bool then "1" else "0"
    _toRaw bool    = if bool then "1" else "0"

-- * SYS FOOS
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

_rawOrNot :: Val2String v => IsRaw -> (v -> String)
_rawOrNot NotRaw = _toString 
_rawOrNot Raw    = _toRaw

_myChunksOf :: Int -> [a] -> [[a]]
_myChunksOf _ [] = []
_myChunksOf n xs = take n xs : _myChunksOf n (drop n xs)

_padList :: Val2String v => IsRaw -> [v] -> [String]
_padList isRaw list
    | null list = []
    | otherwise = map pad listStr
    where
        listStr = map (_rawOrNot isRaw) list
        maxLen  = maximum (map length listStr)
        pad str = str ++ replicate (maxLen - length str) ' '

-- * GEN MACRO CONST
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

_genConst :: Val2String v => IsRaw -> String -> String -> v -> HCompile ()
_genConst isRaw macro name val = do
    fileName <- asks filePath
    sWidth   <- asks constWidth

    liftIO $ appendFile fileName $
        macro ++ padName sWidth name ++
        _rawOrNot isRaw val ++ "\n"

genConst :: Val2String v => String -> String -> v -> HCompile ()
genConst    = _genConst NotRaw

genConstRaw :: Val2String v => String -> String -> v -> HCompile ()
genConstRaw = _genConst Raw

genMacro :: String -> String -> String -> HCompile ()
genMacro macro name body = do
    fileName <- asks filePath

    liftIO $ appendFile fileName $
        macro ++ name ++ body ++ "\n"

-- * GEN TYPE
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

_genType :: Val2String v => IsRaw -> String -> [v] -> (String, String) -> (String, String) -> Int -> HCompile ()
_genType isRaw name fields (sepLine, sepElem) (startStr, endStr) lineLen = do
    fileName <- asks filePath

    liftIO $ appendFile fileName $
        name ++ startStr ++ 
            intercalate sepLine
                        (map (intercalate sepElem)
                        (_myChunksOf lineLen
                        (_padList isRaw fields)))
        ++ endStr

genType :: Val2String v => String -> [v] -> (String, String) -> (String, String) -> Int -> HCompile ()
genType    = _genType NotRaw

genTypeRaw :: Val2String v => String -> [v] -> (String, String) -> (String, String) -> Int -> HCompile ()
genTypeRaw = _genType Raw

-- * GEN TABLE
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

_genTableFoo :: (Val2String v, Enum a, Num a) => 
                IsRaw -> String -> (a -> v) -> (a, a) -> a -> (String, String) -> (String, String) -> Int -> HCompile ()
_genTableFoo isRaw name foo (start, end) step sep limits lineLen =
    _genType isRaw name (map foo [start, start + step .. end]) sep limits lineLen

genTableFoo :: (Val2String v, Enum a, Num a) => 
               String -> (a -> v) -> (a, a) -> a -> (String, String) -> (String, String) -> Int -> HCompile ()
genTableFoo    = _genTableFoo NotRaw

genTableFooRaw :: (Val2String v, Enum a, Num a) =>
                  String -> (a -> v) -> (a, a) -> a -> (String, String) -> (String, String) -> Int -> HCompile ()
genTableFooRaw = _genTableFoo Raw

-- * CHECK FLOAT CLASS
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

class CheckFloat f where
    _infOrNan :: f -> f

instance {-# OVERLAPPABLE #-} RealFloat f => CheckFloat f where
    _infOrNan val
        | isNaN val      = error "expression returned NaN"
        | isInfinite val = error "expression returned Infinite"
        | otherwise      = val

instance CheckFloat String where
    _infOrNan str
        | str == "NaN"       = error "expression returned NaN"
        | str == "Infinity"  = error "expression returned Infinite"
        | str == "-Infinity" = error "expression returned Infinite"
        | otherwise          = str

-- * GEN FLOAT TABLE
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

_genTableFooFloat :: (Val2String v, CheckFloat v, Eq v, Enum a, Fractional a, Eq a, RealFloat a) =>
                     IsRaw -> String -> (a -> v) -> (a, a) -> a -> (String, String) -> (String, String) -> Int -> HCompile ()
_genTableFooFloat isRaw name foo (start, end) step sep limits lineLen 
    | isInfinite step = error "Step should not be Infinity"
    | isNaN step      = error "Step should not be NaN"
    | step == 0       = error "Step must be non-zero"
    | otherwise       =
                        _genType isRaw name 
                        (map (_infOrNan . foo) [start, start + step .. end])
                        sep limits lineLen

genTableFooFloat :: (Val2String v, CheckFloat v, Eq v, Enum a, Fractional a, Eq a, RealFloat a) =>
                    String -> (a -> v) -> (a, a) -> a -> (String, String) -> (String, String) -> Int -> HCompile ()
genTableFooFloat    = _genTableFooFloat NotRaw

genTableFooFloatRaw :: (Val2String v, CheckFloat v, Eq v, Enum a, Fractional a, Eq a, RealFloat a) =>
                       String -> (a -> v) -> (a, a) -> a -> (String, String) -> (String, String) -> Int -> HCompile ()
genTableFooFloatRaw = _genTableFooFloat Raw

-- * COLOR24 TYPE
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

data Color24 = Color24 !Word8 !Word8 !Word8

instance Storable Color24 where
    sizeOf _    = 3
    alignment _ = 1
    peek ptr    = do
        b <- peekByteOff ptr 0
        g <- peekByteOff ptr 1
        r <- peekByteOff ptr 2
        return (Color24 b g r)
    poke _ _ = return ()

instance Show Color24 where
    show (Color24 b g r) = "0x" ++ pad (showHex colorNum "")
      where
        colorNum = (fromIntegral r * 65536) + (fromIntegral g * 256) + fromIntegral b :: Word32
        pad s    = replicate (6 - length s) '0' ++ s

-- * GET BMP INFO
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

getBmpWidth :: FilePath -> HCompile Int
getBmpWidth path = do
    fileData <- liftIO $ B.readFile path

    either (\err -> error $ "Failed to read file: " ++ err)
           (\case {(PalettedRGB8 i _, _) ->
           return (imageWidth i); _      ->
           error "This BMP does not contain an 8 bit palette"})
           (decodeBitmapWithPaletteAndMetadata fileData)

getBmpHeight :: FilePath -> HCompile Int
getBmpHeight path = do
    fileData <- liftIO $ B.readFile path

    either (\err -> error $ "Failed to read file: " ++ err)
           (\case {(PalettedRGB8 i _, _) ->
           return (imageHeight i); _     ->
           error "This BMP does not contain an 8 bit palette"})
           (decodeBitmapWithPaletteAndMetadata fileData)

getBmpPixelsNum :: FilePath -> HCompile Int
getBmpPixelsNum path = do
    fileData <- liftIO $ B.readFile path

    either (\err -> error $ "Failed to read file: " ++ err)
           (\case {(PalettedRGB8 i _, _)            ->
           return (imageWidth i * imageHeight i); _ ->
           error "This BMP does not contain an 8 bit palette"})
           (decodeBitmapWithPaletteAndMetadata fileData)

getBmpSizeByte :: FilePath -> HCompile Int
getBmpSizeByte path = do
    size <- liftIO $ getFileSize path
    return (fromIntegral size)

getBmpPalette :: FilePath -> HCompile [Color24]
getBmpPalette path = do
    fileData <- liftIO $ B.readFile path

    either (\err -> error $ "Failed to read file: " ++ err)
           (\case {(PalettedRGB8 _ p, _)                                                           -> 
           return (V.toList (V.unsafeCast (imageData (palettedAsImage p)) :: V.Vector Color24)); _ -> 
           error "This BMP does not contain an 8 bit palette"})
           (decodeBitmapWithPaletteAndMetadata fileData)

getBmpImage :: FilePath -> HCompile [Word8]
getBmpImage path = do
    fileData <- liftIO $ B.readFile path

    either (\err -> error $ "Failed to read file: " ++ err)
           (\case {(PalettedRGB8 i _, _)      -> 
           return (V.toList (imageData i)); _ -> 
           error "This BMP is not a valid 8 bit image"})
           (decodeBitmapWithPaletteAndMetadata fileData)

-- * GEN TABLE BY BMP INDEXED
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

_bmpPalette :: Image PixelRGB8 -> String -> (String, String) -> (String, String) -> (String, String) -> Int -> HCompile ()
_bmpPalette palette name (field, fSep) sep limits lineLen = do
    pWidth <- asks paletteWidth
    
    _genType Raw name 
                 (map (\(i, f) -> padName pWidth (field ++ show i ++ fSep) ++ show f)
                 (zip [0 :: Int ..] (V.toList (V.unsafeCast (imageData palette) :: V.Vector Color24))))
                 sep limits lineLen

_bmpImage :: Image Pixel8 -> String -> String -> (String, String) -> (String, String) -> Int -> HCompile ()
_bmpImage img name field sep limits lineLen = do
    _genType Raw name
                 (map (\f -> field ++ show f) 
                 (V.toList (imageData img)))
                 sep limits lineLen

genBmpPalette :: FilePath -> String -> (String, String) -> (String, String) -> (String, String) -> Int -> HCompile ()
genBmpPalette path name field sep limits lineLen = do
    fileData <- liftIO $ B.readFile path

    either (\err -> error $ "Failed to read file: " ++ err)
           (\case {(PalettedRGB8 _ p, _)                                    -> 
           _bmpPalette (palettedAsImage p) name field sep limits lineLen; _ -> 
           error "This BMP does not contain an 8 bit palette"})
           (decodeBitmapWithPaletteAndMetadata fileData)

genBmpImage :: FilePath -> String -> String -> (String, String) -> (String, String) -> Int -> HCompile ()
genBmpImage path name field sep limits lineLen = do
    fileData <- liftIO $ B.readFile path

    either (\err -> error $ "Failed to read file: " ++ err)
           (\case {(PalettedRGB8 i _, _)                -> 
           _bmpImage i name field sep limits lineLen; _ -> 
           error "This BMP is not a valid 8 bit image"})
           (decodeBitmapWithPaletteAndMetadata fileData)

-- * RUN HCOMPILE
-- * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

runHCompile :: HCompile h -> HCompileConf -> IO h
runHCompile = runReaderT
