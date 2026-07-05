{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CGenerate (
    CGen,
    Val2String(..),
    cGenInit,
    genDefine,
    runCGen
) where

import Control.Monad.Reader
import System.Directory (removeFile, doesFileExist)
import Control.Monad (when)
import Data.Char (toUpper)

type CGen g = ReaderT FilePath IO g

cGenInit :: CGen ()
cGenInit = do
    fileName <- ask

    liftIO $ doesFileExist fileName >>=
        flip when (removeFile fileName)

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


genDefine :: Val2String v => String -> v -> Bool -> CGen ()
genDefine name val isRaw = do
    fileName <- ask
    liftIO $ appendFile fileName $
        "#define " ++ (map toUpper name) ++ " " ++
        (if isRaw then toRaw val else toString val) ++ "\n"

runCGen :: CGen g -> FilePath -> IO g
runCGen = runReaderT
