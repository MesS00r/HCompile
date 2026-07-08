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
    hCompileEnd,
    runHCompile
) where

import Control.Monad.Reader
import System.Directory     (removeFile, doesFileExist)
import Data.Char            (toUpper, isAlphaNum)
import Data.List            (intercalate)
import Control.Monad        (when)

type HCompile h = ReaderT FilePath IO h

cleanName :: Char -> Char
cleanName ch
    | isAlphaNum ch = toUpper ch
    | otherwise     = '_'

hCompileBegin :: String -> HCompile ()
hCompileBegin includes = do
    fileName <- ask

    liftIO $ doesFileExist fileName >>=
        flip when (removeFile fileName)

    liftIO $ appendFile fileName $
        "#ifndef " ++ map cleanName fileName ++ "\n" ++
        "#define " ++ map cleanName fileName ++ "\n\n" ++

        "#include <stdint.h>\n" ++
        "#include <math.h>\n\n" ++
        includes

send2File :: String -> HCompile ()
send2File str = do
    fileName <- ask
    liftIO $ appendFile fileName str

class Val2String v where
    toString :: v -> String
    toRaw    :: v -> String

instance {-# OVERLAPPABLE #-} (Num v, Show v) => Val2String v where
    toString = show
    toRaw    = show

instance Val2String String where
    toString str = "\"" ++ str ++ "\""
    toRaw str    = str

instance Val2String Char where
    toString ch = "\'" ++ [ch] ++ "\'"
    toRaw ch    = [ch]

instance Val2String Bool where
    toString bool = if bool then "1" else "0"
    toRaw bool    = if bool then "1" else "0"

padName :: String -> String
padName name
    | spaceNeeded > 0 = name ++ replicate spaceNeeded ' '
    | otherwise       = name ++ " "
    where
        spaceNeeded = 15 - length name

genDefine :: Val2String v => String -> v -> HCompile ()
genDefine name val = do
    fileName <- ask
    liftIO $ appendFile fileName $
        "#define " ++ padName name ++ " " ++
        toString val ++ "\n"

genDefineRaw :: Val2String v => String -> v -> HCompile ()
genDefineRaw name val = do
    fileName <- ask
    liftIO $ appendFile fileName $
        "#define " ++ padName name ++ " " ++
        toRaw val ++ "\n"

genDefineMacro :: String -> String -> HCompile ()
genDefineMacro name body = do
    fileName <- ask
    liftIO $ appendFile fileName $
        "#define " ++ name ++ body ++ "\n"

myChunksOf :: Int -> [a] -> [[a]]
myChunksOf _ [] = []
myChunksOf n xs = take n xs : myChunksOf n (drop n xs)

genCType :: Val2String v => String -> [v] -> String -> Int -> HCompile ()
genCType name fields sep lineLen = do
    fileName <- ask
    liftIO $ appendFile fileName $
        name ++ "{\n\t" ++ 
            intercalate (sep ++ "\n\t") 
            (map (intercalate sep) 
            (myChunksOf lineLen 
            (map toRaw fields)))
        ++ "\n};\n"

hCompileEnd :: HCompile ()
hCompileEnd = do
    fileName <- ask
    liftIO $ appendFile fileName $
        "\n#endif // " ++ map cleanName fileName

runHCompile :: HCompile h -> FilePath -> IO h
runHCompile = runReaderT
