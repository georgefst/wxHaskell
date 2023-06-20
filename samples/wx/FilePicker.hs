module Main (main) where

import Control.Concurrent
import Data.Maybe
import Graphics.UI.WX

main :: IO ()
main =
    pickFile
        PickFileOpts
            { windowTitle = "wxHaskell File Picker"
            , message = "Open file"
            , wildcards =
                [ ("All files", ["*"])
                , ("Image files", ["*.bmp", "*.jpg", "*.gif", "*.png"])
                , ("Portable Network Graphics (*.png)", ["*.png"])
                , ("BMP files (*.bmp)", ["*.bmp"])
                , ("JPG files (*.jpg)", ["*.jpg"])
                , ("GIF files (*.gif)", ["*.gif"])
                ]
            , rememberCurrentDir = False
            , allowReadOnly = True
            , directory = Just "~/Desktop"
            , filename = Nothing
            }
        >>= print

pickFile :: PickFileOpts -> IO (Maybe String)
pickFile PickFileOpts{..} = do
    m <- newEmptyMVar
    start do
        f <- frame [text := windowTitle]
        putMVar m
            =<< fileOpenDialog
                f
                rememberCurrentDir
                allowReadOnly
                message
                wildcards
                -- TODO it seems that the empty string is special for these two args?
                -- if so, I prefer `Maybe` for cleaner API, but what about when user passes `Just ""`?
                (fromMaybe "" directory)
                (fromMaybe "" filename)
        close f
    takeMVar m

data PickFileOpts = PickFileOpts
    { windowTitle :: String
    , rememberCurrentDir :: Bool
    , allowReadOnly :: Bool
    , message :: String
    , wildcards :: [(String, [String])]
    , directory :: Maybe FilePath
    , filename :: Maybe FilePath
    }
