-- A command-line tool for managing a simple journal

import Data.Time.Clock
import System.Environment
import Data.Time.Calendar
import System.Directory
import System.IO
import System.Cmd
import System.FilePath
import System.Exit
import Data.List.Split
import Data.List (intersperse)
import qualified Data.Text as T

type Editor = String
type FileName = String
type Suffix = String

main :: IO ExitCode
main = do
    (dir, editor, suffix) <- getConfigs
    args <- getArgs
    timestamp <- getCurrentTime
    let filename = todaysFilename timestamp suffix
    case args of
        [] -> do 
                  exists <- todaysFileExists dir filename
                  if exists
                      then editTodaysFile dir filename editor
                      else createAndEditTodaysFile dir filename editor
        (filename:[]) -> do
                  exists <- doesFileExist (dir ++ pathSeparator:filename)
                  if exists
                      then editFile dir editor filename
                      else error "file not found"
        _ -> error "Usage: journal [filename]"

getConfigs :: IO (FilePath, Editor, Suffix)
getConfigs = do
    handle <- openConfigFile
    contents <- hGetContents handle
    let
        configs = pairsToTuples . filter (\x -> length x == 2) $ map (splitOn "=") (lines contents)
        dir = case maybeDir of
            Nothing -> "."
            Just dir -> dir
            where maybeDir = lookup "dir" configs
        editor = case maybeEditor of
            Nothing -> error "vi"
            Just editor -> editor
            where maybeEditor = lookup "editor" configs
        suffix = case maybeSuffix of
            Nothing -> ""
            Just suffix -> suffix
            where maybeSuffix = lookup "suffix" configs
    return (dir, editor, suffix)

openConfigFile :: IO Handle
openConfigFile = do
    homedir <- getHomeDirectory
    openFile (homedir ++ "/.journalrc") ReadMode

todaysFileExists :: FilePath -> FileName -> IO Bool
todaysFileExists dir filename = do
    doesFileExist (dir ++ pathSeparator:filename)

editTodaysFile :: FilePath -> FileName -> Editor -> IO ExitCode
editTodaysFile dir filename editor = do
    system (editor ++ " " ++ dir ++ pathSeparator:filename)
    
createAndEditTodaysFile :: FilePath -> FileName -> Editor -> IO ExitCode
createAndEditTodaysFile dir filename editor = do
    handle <- openFile (dir ++ pathSeparator:filename) WriteMode
    system (editor ++ " " ++ dir ++ pathSeparator:filename) 

editFile :: FilePath -> FileName -> Editor -> IO ExitCode
editFile dir filename editor = do
    handle <- openFile (dir ++ pathSeparator:filename) ReadMode
    system (editor ++ " " ++ dir ++ pathSeparator:filename) 

-- create a filename of format "yy-mm-dd.txt" from the current UTC time
todaysFilename :: UTCTime -> String -> FileName
todaysFilename timestamp suffix =
    (concat $ intersperse "-" [show (yr tuple), show (mo tuple), show (day tuple)]) ++ suffix
    where tuple = toGregorian $ utctDay timestamp

day :: (Integer, Int, Int) -> Int
day (y,m,d) = d

mo :: (Integer, Int, Int) -> Int
mo (y,m,d) = m

yr :: (Integer, Int, Int) -> Integer
yr (y,m,d) = y

pairsToTuples :: [[a]] -> [(a,a)]
pairsToTuples lst = map (\x -> (x!!0,x!!1)) lst
