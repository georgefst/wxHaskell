{--------------------------------------------------------------------------------
 Copyright (c) Daan Leijen 2003
 wxWindows License.

 A file browser in wxHaskell.
 Demonstrates: 
 - tree control and list control
 - image lists
 - basic directory handling in Haskell 
--------------------------------------------------------------------------------}
module Main where

import Directory
import List( zip3 )
import Graphics.UI.WX
import Graphics.UI.WXCore 


main :: IO ()
main 
  = start gui

{--------------------------------------------------------------------------------
   Images
--------------------------------------------------------------------------------}
imgComputer   = "computer"
imgDisk       = "disk"
imgFile       = "file"
imgHFile      = "hsicon"
imgFolder     = "f_closed"
imgFolderOpen = "f_open"

-- plain names of images
imageNames    
  = [imgComputer,imgDisk,imgFile,imgHFile,imgFolder,imgFolderOpen]

-- file names of the images
imageFiles
  = map (\name -> "../bitmaps/" ++ name ++ ".ico") imageNames

-- get the index of an image
imageIndex :: String -> Int
imageIndex name 
  = case lookup name (zip imageNames [0..]) of
      Just idx  -> idx
      Nothing   -> imageNone

-- (-1) means no image present
imageNone :: Int
imageNone     = (-1)

{--------------------------------------------------------------------------------
   The client data of the directory tree is the full path of the
   tree node. Here we wrap the "unsafe" basic calls into safe wrappers.
--------------------------------------------------------------------------------}
treeCtrlSetItemPath :: TreeCtrl a -> TreeItem -> FilePath -> IO ()
treeCtrlSetItemPath t item path
  = treeCtrlSetItemClientData t item (return ()) path

treeCtrlGetItemPath :: TreeCtrl a -> TreeItem -> IO FilePath 
treeCtrlGetItemPath t item
  = do mbpath <- unsafeTreeCtrlGetItemClientData t item
       case mbpath of
         Just path -> return path
         Nothing   -> return ""


{--------------------------------------------------------------------------------
   GUI
--------------------------------------------------------------------------------}
gui :: IO ()
gui
  = do -- main gui elements: frame, panel
       f <- frame [text := "File browser", image := "../bitmaps/wxwin.ico"]
       
       -- panel: just for the nice grey color
       p <- panel f []
      
       -- image list
       imagePaths <- mapM getAbsoluteFilePath imageFiles  -- make relative to application
       images     <- imageListFromFiles (sz 16 16) imagePaths

       -- splitter window between directory tree and file view.
       s <- splitterWindow p []

       -- initialize tree control
       t <- treeCtrl s []
       treeCtrlAssignImageList t images  {- 'assign' deletes the imagelist on delete -}
       
       -- set top node
       top <- treeCtrlAddRoot t "System" (imageIndex imgComputer) imageNone objectNull
       treeCtrlSetItemPath t top ""

       -- add root directory
       (rootPath,rootName) <- getRootDir        
       root <- treeCtrlAppendItem t top rootName (imageIndex imgDisk) imageNone objectNull 
       treeCtrlSetItemPath t root rootPath 
       treeCtrlAddSubDirs t root

       -- expand top node
       treeCtrlExpand t top

       -- list control
       l  <- listCtrl s [columns := [("Name",AlignLeft,140),("Permissions",AlignLeft,80),("Date",AlignLeft,100)]]
       listCtrlSetImageList l images wxIMAGE_LIST_SMALL
       
       -- status bar
       status <- statusField [text := "wxHaskell file browser example"]

       -- install event handlers
       set t [on treeEvent := onTreeEvent t l status]
       set l [on listEvent := onListEvent l status]

       -- specify layout
       set f [layout     := container p $ margin 5 $ 
                            fill  $ vsplit s 5 {- sash width -} 160 {- left pane width -} (widget t) (widget l)
             ,statusBar  := [status]
             ,clientSize := sz 500 300
             ]
{-       
       -- Give the tree control 1/3 of the view
       ssize <- get s clientSize
       splitterWindowSplitVertically s t l ((sizeW ssize) `div` 3)
-}
       return ()

{--------------------------------------------------------------------------------
   On tree event
--------------------------------------------------------------------------------}
onTreeEvent :: TreeCtrl a -> ListCtrl b -> StatusField -> EventTree -> IO ()
onTreeEvent t l status event
  = case event of
      TreeItemExpanding item veto  | treeItemIsOk item
        -> do wxcBeginBusyCursor
              treeCtrlChildrenAddSubDirs t item
              wxcEndBusyCursor
              propagateEvent
      TreeSelChanged item olditem  | treeItemIsOk item
        -> do wxcBeginBusyCursor
              path <- treeCtrlGetItemPath t item
              set status [text := path]
              listCtrlShowDir l path
              wxcEndBusyCursor
              propagateEvent
      other
        -> propagateEvent

onListEvent :: ListCtrl a -> StatusField -> EventList -> IO ()
onListEvent l status event
  = case event of
      ListItemSelected item
        -> do count <- listCtrlGetSelectedItemCount l
              set status [text := (show count ++ " item" ++ (if count /= 1 then "s" else "") ++ " selected") ]
              propagateEvent
      other
        -> propagateEvent
  
{--------------------------------------------------------------------------------
   View directory files
--------------------------------------------------------------------------------}
listCtrlShowDir :: ListCtrl a -> FilePath -> IO ()
listCtrlShowDir listCtrl path
  = do listCtrlDeleteAllItems listCtrl
       contents <- getDirectoryContents path
       let paths = map (\dir -> path ++ dir ++ "/") contents
       mapM_ (listCtrlAddFile listCtrl) (zip3 [0..] contents paths)
  `catch` \err -> return ()

listCtrlAddFile l (idx,fname,fpath)
  = do isdir <- doesDirectoryExist fpath `catch` \err -> return False
       perm  <- getPermissions fpath
       time  <- getModificationTime fpath
       let image = imageIndex (if isdir 
                                then imgFolder 
                                else if (extension fname == "hs")
                                      then imgHFile
                                      else imgFile)
       listCtrlInsertItemWithLabel l idx fpath image
       listCtrlSetItem l idx 0 {- name -} fname image
       listCtrlSetItem l idx 1 {- perm -} (showPerm perm) imageNone
       listCtrlSetItem l idx 2 {- date -} (show time) imageNone

extension fname
  | elem '.' fname  = reverse (takeWhile (/='.') (reverse fname))
  | otherwise       = ""

showPerm perm
  = [if readable perm then 'r' else '-'
    ,if writable perm then 'w' else '-'
    ,if executable perm then 'x' else '-'
    ,if searchable perm then 's' else '-'
    ]

{--------------------------------------------------------------------------------
   Directory tree helpers
--------------------------------------------------------------------------------}
treeCtrlChildrenAddSubDirs :: TreeCtrl a -> TreeItem -> IO ()
treeCtrlChildrenAddSubDirs t parent
  = do children <- treeCtrlGetChildren t parent
       mapM_ (treeCtrlAddSubDirs t) children

treeCtrlAddSubDirs :: TreeCtrl a -> TreeItem -> IO ()
treeCtrlAddSubDirs t parent
  = do fpath <- treeCtrlGetItemPath t parent
       dirs  <- getSubdirs fpath
       treeCtrlDeleteChildren t parent
       mapM_ addChild dirs
       treeCtrlSetItemHasChildren t parent (not (null dirs))
  where
    addChild (path,name)
      = do item <- treeCtrlAppendItem t parent name (imageIndex imgFolder) (imageIndex imgFolderOpen) objectNull
           treeCtrlSetItemPath t item path

{--------------------------------------------------------------------------------
   General directory operations
--------------------------------------------------------------------------------}

-- Return the sub directories of a certain directory as a tuple: the full path and the directory name.
getSubdirs :: FilePath -> IO [(FilePath,FilePath)]
getSubdirs fpath
  = do contents  <- getDirectoryContents fpath `catch` \err -> return []
       let names = filter (\dir -> head dir /= '.') contents
           paths = map (\dir -> fpath ++ dir ++ "/") names
       isdirs    <- mapM (\dir -> doesDirectoryExist dir `catch` \err -> return False) paths
       let dirs  = [(path,name) | (isdir,(path,name)) <- zip isdirs (zip paths names), isdir]
       return dirs
       

-- Return the root directory as a tuple: the full path and name.
getRootDir :: IO (FilePath,FilePath)
getRootDir
  = do current <- getCurrentDirectory
       let isDirSep c = (c == '\\' || c == '/')
           rootName  = takeWhile (not . isDirSep) current
           rootPath  = rootName ++ "/"
       exist <- do{ getDirectoryContents rootPath; return True } `catch` \err -> return False
       if exist
        then return (rootPath,rootName)
        else return (current ++ "/", reverse (takeWhile (not . isDirSep) (reverse current)))
