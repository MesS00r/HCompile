{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HCompile (
    HCompile,
    Val2String(..),
    hCompileBegin,
    send2File,
    genDefine,
    genDefineRaw,
    genDefineMacro,
    genCType,
    genCTypeRaw,
    genTableExp,
    genTableExpRaw,
    genTableExpFloat,
    genTableExpFloatRaw,
    hCompileEnd,
    runHCompile
) where

import Control.Monad.Reader
import System.Directory     (removeFile, doesFileExist)
import Data.Char            (toUpper, isAlphaNum)
import Data.List            (intercalate)
import Control.Monad        (when)

type HCompile h = ReaderT FilePath IO h

_cleanName :: Char -> Char
_cleanName ch
    | isAlphaNum ch = toUpper ch
    | otherwise     = '_'

hCompileBegin :: String -> HCompile ()
hCompileBegin includes = do
    fileName <- ask

    liftIO $ doesFileExist fileName >>=
        flip when (removeFile fileName)

    liftIO $ appendFile fileName $
        "#ifndef " ++ map _cleanName fileName ++ "\n" ++
        "#define " ++ map _cleanName fileName ++ "\n\n" ++
        includes

send2File :: String -> HCompile ()
send2File str = do
    fileName <- ask
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
    fileName <- ask
    liftIO $ appendFile fileName $
        "#define " ++ _padName name 15 ++ " " ++
        _rawOrNot isRaw val ++ "\n"

genDefine :: Val2String v => String -> v -> HCompile ()
genDefine    = _genDefine NotRaw

genDefineRaw :: Val2String v => String -> v -> HCompile ()
genDefineRaw = _genDefine Raw

genDefineMacro :: String -> String -> HCompile ()
genDefineMacro name body = do
    fileName <- ask
    liftIO $ appendFile fileName $
        "#define " ++ name ++ body ++ "\n"

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

_genCType :: Val2String v => IsRaw -> String -> [v] -> String -> Int -> HCompile ()
_genCType isRaw name fields sep lineLen = do
    fileName <- ask
    liftIO $ appendFile fileName $
        name ++ "{\n\t" ++ 
            intercalate (sep ++ "\n\t")
            (map (intercalate sep)
            (_myChunksOf lineLen
            (_padList isRaw fields)))
        ++ "\n};\n"

genCType :: Val2String v => String -> [v] -> String -> Int -> HCompile ()
genCType    = _genCType NotRaw

genCTypeRaw :: Val2String v => String -> [v] -> String -> Int -> HCompile ()
genCTypeRaw = _genCType Raw

_genCTypeRawOrNot :: Val2String v => IsRaw -> (String -> [v] -> String -> Int -> HCompile ())
_genCTypeRawOrNot NotRaw = genCType
_genCTypeRawOrNot Raw    = genCTypeRaw

_genTableExp :: (Val2String v, Enum a, Num a) => IsRaw -> String -> (a -> v) -> a -> Int -> HCompile ()
_genTableExp isRaw name foo size lineLen =
    _genCTypeRawOrNot isRaw (name ++ "[] = ") (map foo [0..size]) ", " lineLen

genTableExp :: (Val2String v, Enum a, Num a) => String -> (a -> v) -> a -> Int -> HCompile ()
genTableExp    = _genTableExp NotRaw

genTableExpRaw :: (Val2String v, Enum a, Num a) => String -> (a -> v) -> a -> Int -> HCompile ()
genTableExpRaw = _genTableExp Raw

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

_genTableExpFloat :: (Val2String v, CheckFloat v, Eq v, Enum a, Fractional a, Eq a, RealFloat a) =>
                    IsRaw -> String -> (a -> v) -> (a, a) -> a -> Int -> HCompile ()
_genTableExpFloat isRaw name foo (start, end) step lineLen 
    | isInfinite step = error "Step should not be Infinity"
    | isNaN step      = error "Step should not be NaN"
    | step == 0       = error "Step must be non-zero"
    | otherwise       =
                        _genCTypeRawOrNot isRaw (name ++ "[] = ")
                        (map _infOrNan (map foo [start, start + step .. end])) 
                        ", " lineLen

genTableExpFloat :: (Val2String v, CheckFloat v, Eq v, Enum a, Fractional a, Eq a, RealFloat a) =>
                    String -> (a -> v) -> (a, a) -> a -> Int -> HCompile ()
genTableExpFloat    = _genTableExpFloat NotRaw

genTableExpFloatRaw :: (Val2String v, CheckFloat v, Eq v, Enum a, Fractional a, Eq a, RealFloat a) =>
                    String -> (a -> v) -> (a, a) -> a -> Int -> HCompile ()
genTableExpFloatRaw = _genTableExpFloat Raw

-- genTableBmp ::
-- genTableBmp = 

hCompileEnd :: HCompile ()
hCompileEnd = do
    fileName <- ask
    liftIO $ appendFile fileName $
        "\n#endif // " ++ map _cleanName fileName

runHCompile :: HCompile h -> FilePath -> IO h
runHCompile = runReaderT
