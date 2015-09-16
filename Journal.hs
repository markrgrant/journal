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
import qualified Data.Text as T

type Editor = String

main :: IO ExitCode
main = do
    (dir, editor) <- getConfigs
    args <- getArgs
    case args of
        [] -> do 
                  exists <- todaysFileExists dir
                  if exists
                      then editTodaysFile dir editor
                      else createAndEditTodaysFile dir editor
        (filename:[]) -> do
                  exists <- doesFileExist (dir ++ pathSeparator:filename)
                  if exists
                      then editFile dir editor filename
                      else error "file not found"
        _ -> error "Usage: journal [filename]"

getConfigs :: IO (FilePath, Editor)
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
    return (dir, editor)

openConfigFile :: IO Handle
openConfigFile = do
    homedir <- getHomeDirectory
    openFile (homedir ++ "/.journalrc") ReadMode

todaysFileExists :: FilePath -> IO Bool
todaysFileExists dir = do
    filename <- todaysFilename 
    doesFileExist (dir ++ pathSeparator:filename)

editTodaysFile :: FilePath -> Editor -> IO ExitCode
editTodaysFile dir editor = do
    filename <- todaysFilename
    system (editor ++ " " ++ dir ++ pathSeparator:filename)
    
createAndEditTodaysFile :: FilePath -> Editor -> IO ExitCode
createAndEditTodaysFile dir editor = do
    filename <- todaysFilename
    handle <- openFile (dir ++ pathSeparator:filename) WriteMode
    system (editor ++ " " ++ dir ++ pathSeparator:filename) 

editFile :: FilePath -> Editor -> String -> IO ExitCode
editFile dir editor filename = do
    filename <- todaysFilename
    handle <- openFile (dir ++ pathSeparator:filename) ReadMode
    system (editor ++ " " ++ dir ++ pathSeparator:filename) 

-- return a filename of format "yy-mm-dd.txt" where yy-mm-dd are from today's date
todaysFilename :: IO String
todaysFilename = do
    tuple <- getCurrentTime >>= return . toGregorian . utctDay
    return ((show (yr tuple)) ++ "-" ++ (show (mo tuple)) ++ "-" ++ (show (day tuple)) ++ ".txt")

day :: (Integer, Int, Int) -> Int
day (y,m,d) = d

mo :: (Integer, Int, Int) -> Int
mo (y,m,d) = m

yr :: (Integer, Int, Int) -> Integer
yr (y,m,d) = y

pairsToTuples :: [[a]] -> [(a,a)]
pairsToTuples lst = map (\x -> (x!!0,x!!1)) lst
