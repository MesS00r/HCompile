{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HCompile (
    HCompileConf(..),
    HCompile,
    Val2String(..),
    hCompileBegin,
    send2File,
    genDefine,
    genDefineRaw,
    genMacro,
    genType,
    genTypeRaw,
    genTableFoo,
    genTableFooRaw,
    genTableFooFloat,
    genTableFooFloatRaw,
    hCompileEnd,
    runHCompile
) where

import Control.Monad.Reader
import System.Directory     (removeFile, doesFileExist)
import Data.Char            (toUpper, isAlphaNum)
import Data.List            (intercalate)
import Control.Monad        (when)

-- import Codec.Picture
-- import Codec.Picture.Types
-- import qualified Data.ByteString as B
-- import qualified Data.Vector.Storable as V

data HCompileConf = HCompileConf {
    defineWidth :: Int,
    filePath    :: FilePath
    }
type HCompile h = ReaderT HCompileConf IO h

_cleanName :: Char -> Char
_cleanName ch
    | isAlphaNum ch = toUpper ch
    | otherwise     = '_'

hCompileBegin :: String -> HCompile ()
hCompileBegin includes = do
    fileName <- asks filePath

    liftIO $ doesFileExist fileName >>=
        flip when (removeFile fileName)

    liftIO $ appendFile fileName $
        "#ifndef " ++ map _cleanName fileName ++ "\n" ++
        "#define " ++ map _cleanName fileName ++ "\n\n" ++
        includes

send2File :: String -> HCompile ()
send2File str = do
    fileName <- asks filePath
    liftIO $ appendFile fileName str

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

_rawOrNot :: Val2String v => IsRaw -> (v -> String)
_rawOrNot NotRaw = _toString 
_rawOrNot Raw    = _toRaw

_padName :: String -> Int -> String
_padName name spaces
    | spaceNeeded > 0 = name ++ replicate spaceNeeded ' '
    | otherwise       = name ++ " "
    where
        spaceNeeded = spaces - length name

_genDefine :: Val2String v => IsRaw -> String -> v -> HCompile ()
_genDefine isRaw name val = do
    fileName <- asks filePath
    dWidth   <- asks defineWidth
    liftIO $ appendFile fileName $
        "#define " ++ _padName name dWidth ++ " " ++
        _rawOrNot isRaw val ++ "\n"

genDefine :: Val2String v => String -> v -> HCompile ()
genDefine    = _genDefine NotRaw

genDefineRaw :: Val2String v => String -> v -> HCompile ()
genDefineRaw = _genDefine Raw

genMacro :: String -> String -> HCompile ()
genMacro name body = do
    fileName <- asks filePath
    liftIO $ appendFile fileName $
        "#define " ++ name ++ " " ++ body ++ "\n"

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

_genType :: Val2String v => IsRaw -> String -> [v] -> String -> (String, String) -> Int -> HCompile ()
_genType isRaw name fields sep (startStr, endStr) lineLen = do
    fileName <- asks filePath
    liftIO $ appendFile fileName $
        name ++ startStr ++ 
            intercalate (sep ++ "\n\t")
            (map (intercalate sep)
            (_myChunksOf lineLen
            (_padList isRaw fields)))
        ++ endStr

genType :: Val2String v => String -> [v] -> String -> (String, String) -> Int -> HCompile ()
genType    = _genType NotRaw

genTypeRaw :: Val2String v => String -> [v] -> String -> (String, String) -> Int -> HCompile ()
genTypeRaw = _genType Raw

_genTableFoo :: (Val2String v, Enum a, Num a) => 
                IsRaw -> String -> (a -> v) -> a -> String -> (String, String) -> Int -> HCompile ()
_genTableFoo isRaw name foo size sep limits lineLen =
    _genType isRaw name (map foo [0..size]) sep limits lineLen

genTableFoo :: (Val2String v, Enum a, Num a) => 
               String -> (a -> v) -> a -> String -> (String, String) -> Int -> HCompile ()
genTableFoo    = _genTableFoo NotRaw

genTableFooRaw :: (Val2String v, Enum a, Num a) =>
                  String -> (a -> v) -> a -> String -> (String, String) -> Int -> HCompile ()
genTableFooRaw = _genTableFoo Raw

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

_genTableFooFloat :: (Val2String v, CheckFloat v, Eq v, Enum a, Fractional a, Eq a, RealFloat a) =>
                     IsRaw -> String -> (a -> v) -> (a, a) -> a -> String -> (String, String) -> Int -> HCompile ()
_genTableFooFloat isRaw name foo (start, end) step sep limits lineLen 
    | isInfinite step = error "Step should not be Infinity"
    | isNaN step      = error "Step should not be NaN"
    | step == 0       = error "Step must be non-zero"
    | otherwise       =
                        _genType isRaw name 
                        (map (_infOrNan . foo) [start, start + step .. end])
                        sep limits lineLen

genTableFooFloat :: (Val2String v, CheckFloat v, Eq v, Enum a, Fractional a, Eq a, RealFloat a) =>
                    String -> (a -> v) -> (a, a) -> a -> String -> (String, String) -> Int -> HCompile ()
genTableFooFloat    = _genTableFooFloat NotRaw

genTableFooFloatRaw :: (Val2String v, CheckFloat v, Eq v, Enum a, Fractional a, Eq a, RealFloat a) =>
                       String -> (a -> v) -> (a, a) -> a -> String -> (String, String) -> Int -> HCompile ()
genTableFooFloatRaw = _genTableFooFloat Raw

-- readBmp :: FilePath -> IO ([PixelRGB8], [Pixel8])

-- genTableBmpIdxd ::
-- genTableBmpIdxd = 

hCompileEnd :: HCompile ()
hCompileEnd = do
    fileName <- asks filePath
    liftIO $ appendFile fileName $
        "\n#endif // " ++ map _cleanName fileName

runHCompile :: HCompile h -> HCompileConf -> IO h
runHCompile = runReaderT
