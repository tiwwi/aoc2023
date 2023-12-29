{-# LANGUAGE OverloadedStrings #-}
module AOCFiles (getAOCInput) where

import qualified Data.ByteString.Char8 as B
import Network.HTTP.Simple
import Text.Printf
import System.Directory

import Control.Monad (unless)

getToken :: IO B.ByteString
getToken = B.strip <$> B.readFile "app/session_token.cookie"

pullAOCInputUncached :: FilePath -> Int -> Int -> IO ()
pullAOCInputUncached target year day = do
    let url = printf "https://adventofcode.com/%d/day/%d/input" year day
    baseRequest <- parseRequest url 
    token <- getToken
    let finalRequest = addRequestHeader "cookie" ("session=" <> token) baseRequest
    input <- getResponseBody <$> httpBS finalRequest
    B.writeFile target input

fileExistsNonEmpty :: FilePath -> IO Bool
fileExistsNonEmpty path = do
    exists <- doesFileExist path
    if exists
        then (>0) <$> getFileSize path
        else pure False

getAOCInput :: FilePath -> Int -> IO ()
getAOCInput target day = do
    exists <- fileExistsNonEmpty target
    unless exists $ pullAOCInputUncached target 2023 day
