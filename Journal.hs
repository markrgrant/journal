-- A command-line tool for managing a simple journal

import Data.Time.Clock
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
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
type Config = (FilePath, Editor, Suffix)


defaultDir = "."
defaultEditor = "vi"
defaultSuffix = ".txt"


main :: IO ExitCode
main = do
    args      <- getArgs
    timestamp <- getCurrentTime
    configs   <- readConfigs
    let (dir, editor, suffix) = parseConfigs configs
        filename = todaysFilename timestamp suffix
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


readConfigs :: IO String
readConfigs = openConfigFile >>= hGetContents


openConfigFile :: IO Handle
openConfigFile = do
    homedir <- getHomeDirectory
    openFile (homedir ++ "/.journalrc") ReadMode


parseConfigs :: String -> Config
parseConfigs contents = let
    configs = pairsToTuples . filter (\x -> length x == 2) $ map (splitOn "=") (lines contents)
    dir = fromMaybe defaultDir $ lookup "dir" configs
    editor = fromMaybe defaultEditor $ lookup "editor" configs
    suffix = fromMaybe defaultSuffix $ lookup "suffix" configs
    in (dir, editor, suffix)
    where pairsToTuples :: [[a]] -> [(a,a)]
          pairsToTuples = map (\x -> (head x, x!!1))


todaysFileExists :: FilePath -> FileName -> IO Bool
todaysFileExists dir filename = 
    doesFileExist $ dir ++ pathSeparator:filename


editTodaysFile :: FilePath -> FileName -> Editor -> IO ExitCode
editTodaysFile dir filename editor =
    system $ editor ++ " " ++ dir ++ pathSeparator:filename

    
createAndEditTodaysFile :: FilePath -> FileName -> Editor -> IO ExitCode
createAndEditTodaysFile dir filename editor = do
    handle <- openFile (dir ++ pathSeparator:filename) WriteMode
    system $ editor ++ " " ++ dir ++ pathSeparator:filename


editFile :: FilePath -> FileName -> Editor -> IO ExitCode
editFile dir filename editor = do
    handle <- openFile (dir ++ pathSeparator:filename) ReadMode
    system $ editor ++ " " ++ dir ++ pathSeparator:filename

-- create a filename of format "yy-mm-dd.txt" from the current UTC time
todaysFilename :: UTCTime -> String -> FileName
todaysFilename timestamp suffix =
    intercalate "-" [show (yr tuple), show (mo tuple), show (day tuple)] ++ suffix
    where tuple = toGregorian $ utctDay timestamp
          day (y,m,d) = d
          mo  (y,m,d) = m
          yr  (y,m,d) = y
