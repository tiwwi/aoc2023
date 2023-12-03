module Helpers (readT, readMaybeT) where

import qualified Data.Text as T
import Text.Read (readMaybe)

readT :: Read a => T.Text -> a
readT = read . T.unpack

readMaybeT :: Read a => T.Text -> Maybe a
readMaybeT = readMaybe . T.unpack
