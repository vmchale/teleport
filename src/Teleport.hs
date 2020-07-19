{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Composition  hiding ((&))
import           Control.Monad
import           Data.Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor         (($>))
import           Data.List
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       ((<>))
import           Data.Version
import           GHC.Generics         (Generic)
import           Options.Applicative
import           Paths_shift
import           System.Console.ANSI
import           System.Directory     (canonicalizePath, doesDirectoryExist,
                                       doesFileExist, getCurrentDirectory,
                                       setCurrentDirectory)
import           System.Environment
import           System.Exit          (ExitCode (..), die, exitWith)
import           System.FilePath      ((</>))

data AddOptions = AddOptions { folderPath :: Maybe String,
                               addname    :: String }

newtype RemoveOptions = RemoveOptions { removename :: String }

newtype GotoOptions = GotoOptions { gotoname :: String }

-- | data type for command
data Command = Display
             | Add AddOptions
             | Remove RemoveOptions
             | Goto GotoOptions
             | Replace String
             | Here

-- an abstract entity representing a point to which we can warp to
data WarpPoint = WarpPoint { _name          :: String
                           , _absFolderPath :: String }
               deriving (Generic, Binary)

keyByDir :: WarpPoint -> (String, String)
keyByDir (WarpPoint x y) = (y, x)

-- the main data that is loaded from config
newtype WarpData = WarpData { _warpPoints :: [WarpPoint] }
                 deriving (Generic, Binary)

defaultWarpData :: WarpData
defaultWarpData = WarpData []

mapWarpPoints :: ([WarpPoint] -> [WarpPoint]) -> WarpData -> WarpData
mapWarpPoints f (WarpData ws) = WarpData (f ws)

main :: IO ()
main = execParser opts >>= run
    where versionInfo = infoOption ("teleport version: " ++ showVersion version) (short 'v' <> long "version" <> help "Show version")
          opts        = info (helper <*> versionInfo <*> parseCommand)
                             (fullDesc
                             <> progDesc "use warp to quickly setup warp points and move between them"
                             <> header "Warp: move around your filesystem")

decodeWarpData :: FilePath -> IO WarpData
decodeWarpData = fmap decode . (fmap BSL.fromStrict . BS.readFile)

loadWarpData :: FilePath -> IO WarpData
loadWarpData configFilePath = doesFileExist configFilePath >>= \exists ->
    if exists then decodeWarpData configFilePath
    else saveWarpData configFilePath defaultWarpData $> defaultWarpData

saveWarpData :: FilePath -> WarpData -> IO ()
saveWarpData configFilePath warpData =
    let dataBytestring = encode warpData in
        BSL.writeFile configFilePath dataBytestring

warpDataPath :: IO FilePath
warpDataPath = do
    home <- getEnv "HOME"
    pure (home </> ".warpdata")

warpnameParser :: Parser String
warpnameParser = argument str
    (metavar "NAME"
    <> help "name of the warp point")

parseAddCommand :: Parser Command
parseAddCommand = Add .* AddOptions <$> folderParser <*> warpnameParser

folderParser :: Parser (Maybe String)
folderParser = optional $ strOption
    (long "path"
    <> short 'p'
    <> metavar "FOLDER"
    <> help "path to the folder to warp to")

parseRemoveCommand :: Parser Command
parseRemoveCommand = Remove . RemoveOptions <$> warpnameParser

parseGotoCommand :: Parser Command
parseGotoCommand = Goto . GotoOptions <$> warpnameParser

parseReplaceCommand :: Parser Command
parseReplaceCommand = Replace <$> warpnameParser

parseCommand :: Parser Command
parseCommand = hsubparser
    (command "add" (info parseAddCommand (progDesc "add a warp point"))
    <> command "list" (info (pure Display) (progDesc "list all warp points"))
    <> command "del" (info parseRemoveCommand (progDesc "delete a warp point"))
    <> command "replace" (info parseReplaceCommand (progDesc "replace a warp point"))
    <> command "to" (info parseGotoCommand (progDesc "go to a created warp point"))
    <> command "here" (info (pure Here) (progDesc "list relevant warp point for current dir")))

setErrorColor :: IO ()
setErrorColor = setSGR [SetColor Foreground Vivid Red]

colorWhen :: IO () -> IO ()
colorWhen act = do
    useColor <- fromMaybe "1" <$> lookupEnv "CLICOLOR"
    if useColor /= "0" then act else mempty

warpPointPrint :: WarpPoint -> IO ()
warpPointPrint warpPoint = do
    colorWhen $ setSGR [SetColor Foreground Dull White]
    putStr (_name warpPoint)
    colorWhen $ setSGR [SetColor Foreground Vivid Blue]
    putStr $ "\t" <> _absFolderPath warpPoint <> "\n"

folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = setErrorColor *>
    die ("unable to find folder: " ++ show path)

needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = setErrorColor *>
    die ("expected folder, not file: " ++ show path)

dieIfFolderNotFound :: FilePath -> IO ()
dieIfFolderNotFound path = sequence_
    [ flip when (needFolderNotFileError path) =<< doesFileExist path
    , flip unless (folderNotFoundError path) =<< doesDirectoryExist path ]

dieWarpPointExists :: WarpPoint -> IO ()
dieWarpPointExists warpPoint  = sequence_
    [ setErrorColor
    , putStrLn $ "warp point " <> _name warpPoint <> " already exists:\n"
    , warpPointPrint warpPoint
    ]

runAdd :: AddOptions -> IO ()
runAdd AddOptions{..} = do
    dieIfFolderNotFound . fromMaybe "./" $ folderPath
    putStrLn "folder exists, loading warp data..."
    warpData <- loadWarpData =<< warpDataPath
    _absFolderPath <- canonicalizePath . fromMaybe "./" $ folderPath
    let existingWarpPoint = find ((==addname) . _name) (_warpPoints warpData)
    case existingWarpPoint of
        Just warpPoint -> dieWarpPointExists warpPoint
        Nothing -> do
                        putStrLn "creating warp point: \n"
                        let newWarpPoint = WarpPoint addname _absFolderPath
                        warpPointPrint newWarpPoint
                        let newWarpData = mapWarpPoints (newWarpPoint:) warpData
                        flip saveWarpData newWarpData =<< warpDataPath

runDisplay :: IO ()
runDisplay = do
    warpData <- loadWarpData =<< warpDataPath
    forM_ (_warpPoints warpData) warpPointPrint

runHere :: IO ()
runHere = do
    warpData <- loadWarpData =<< warpDataPath
    pwd <- getCurrentDirectory
    case lookup pwd (keyByDir <$> _warpPoints warpData) of
        Just y  -> do
            { colorWhen $ setSGR [SetColor Foreground Dull White]
            ; putStrLn y
            }
        Nothing -> dieWarpPointNotFound pwd

dieWarpPointNotFound :: String -> IO ()
dieWarpPointNotFound wStr = setErrorColor *> die
    (wStr <> " warp point not found")

runRemove :: RemoveOptions -> IO ()
runRemove RemoveOptions{..} = do
    warpPath <- warpDataPath
    warp <- loadWarpData warpPath
    let wantedWarpPoint = find ((/= removename) . _name) (_warpPoints warp)
    case wantedWarpPoint of
        Nothing -> dieWarpPointNotFound removename
        Just _ -> saveWarpData warpPath
            (mapWarpPoints (filter ((/= removename) . _name)) warp)

runGoto :: GotoOptions -> IO ()
runGoto GotoOptions{..} = do
    warpPath <- warpDataPath
    warp <- loadWarpData warpPath
    let wantedWarpPoint = find ((== gotoname) . _name) (_warpPoints warp)
    case wantedWarpPoint of
        Nothing -> dieWarpPointNotFound gotoname
        Just warpPoint -> do
                             putStrLn $ _absFolderPath warpPoint
                             setCurrentDirectory $ _absFolderPath warpPoint
                             exitWith (ExitFailure 2)

run :: Command -> IO ()
run (Add addOpt)       = runAdd addOpt
run Display            = runDisplay
run (Remove removeOpt) = runRemove removeOpt
run (Replace strR)     = runRemove (RemoveOptions strR) *> runAdd (AddOptions Nothing strR)
run (Goto gotoOpt)     = runGoto gotoOpt
run Here = runHere
