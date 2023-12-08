module Helpers (readT, readMaybeT, decimalToInt, quickParseT, failsIf) where

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as T
import Text.Read (readMaybe)
import Control.Monad (MonadPlus, guard)

readT :: Read a => T.Text -> a
readT = read . T.unpack

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack

decimalToInt :: [Int] -> Int
decimalToInt = foldl go 0
    where go acc x = 10*acc + x

quickParseT :: T.Parser a -> T.Text -> a
quickParseT p txt = case T.parseOnly p txt of
                        Right a -> a
                        Left err -> error err

failsIf :: MonadPlus m => m a -> (a -> Bool) -> m a
failsIf m f = do
    x <- m
    guard $ f x
    return x
